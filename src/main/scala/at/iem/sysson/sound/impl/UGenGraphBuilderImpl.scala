package at.iem.sysson
package sound
package impl

import de.sciss.synth
import synth.impl.BasicUGenGraphBuilder
import synth._
import ugen.ControlProxyLike
import at.iem.sysson.graph
import graph.{Var, SelectedLike}
import Implicits._

object UGenGraphBuilderImpl {
  def apply(sonif: Sonification, sg: SynthGraph): UGenGraphBuilder.Result = new Impl(sonif).build(sg)

  // OBSOLETE
  def bufCtlName (key: String): String = "$son_" + key
  //  def rateCtlName(key: String): String = "$son_" + key

  // final val diskTrigID      = 0
  final val diskUsesInterp  = false
  final val diskPad         = if (diskUsesInterp) 4 else 0

  private final class Impl(sonif: Sonification) extends BasicUGenGraphBuilder with UGenGraphBuilder {
    import UGenGraphBuilder.Section

    override def toString     = s"UGenGraphBuilder(${sonif.name})@" + hashCode.toHexString

    // OBSOLETE
    private var usedMappings  = Set.empty[String]

    private var sections      = Vec.empty[Section]
    private var userValues    = Set.empty[UGenGraphBuilder.UserValue]

    def getMatrixInSource(m: MatrixIn): SonificationSource = {
      val key = m.key
      sonif.mapping.getOrElse(key, throw Sonification.MissingInput(key))
    }

    private def withVariable[A](range: SelectedLike)(fun: (String, VariableSection) => A): A =
      range.variable.find(sonif.variableMap)(_._2.variable).fold[A] {
        sys.error(s"Selection for ${range.variable} not specified")
      } (tup => fun(tup._1, tup._2))

    private def require1D(range: SelectedLike, section: VariableSection): Unit =
      require(section.rank == 1, s"Selection for ${range.variable} must be one-dimensional")

    private def ctlNameFromSection(section: VariableSection): String =
      section.shape.mkString(s"$$var_${section.name}_", "_", "")

    def addScalarUserValue(value: graph.UserValue): GE = {
      val ctl = s"$$user_${value.key}"
      userValues += UGenGraphBuilder.UserValue(controlName = ctl, peer = value)
      ctl.ir(value.default)
    }

    def addScalarSelection(range: SelectedLike): GE = withVariable(range) { (name, section) =>
      require1D(range, section)

      val ctl         = ctlNameFromSection(section)
      val uSect       = Section(controlName = ctl, peer = section, streamDim = -1, streamID = -1)
      val numFramesL  = section.size
      require(numFramesL <= 4096, s"Scalar selection too large ($numFramesL > 4096)")
      val numFrames   = numFramesL.toInt
      sections      :+= uSect

      import ugen._
      Index.kr(buf = ctl.ir, in = 0 until numFrames) // XXX TODO or should enforce audio-rate?
    }

    def addAudioSelection(range: SelectedLike, freq: GE): GE = withVariable(range) { (name, section) =>
      require1D(range, section)

      val ctl = ctlNameFromSection(section)
      addAudioMatrix(controlName = ctl, freq = freq, section = section, streamDim = 0 /* there is only 1D */)
    }

    private def addAudioMatrix(controlName: String, freq: GE, section: VariableSection, streamDim: Int): GE = {
      val trigID      = sections.size // XXX TODO: bit hackish
      val uSect       = Section(controlName = controlName, peer = section, streamDim = streamDim, streamID = trigID)
      sections      :+= uSect

      import ugen._
      val numChannels = (section.size / section.shape(streamDim)).toInt

      val inBuf       = controlName.ir
      val bufRate     = freq // BufSampleRate.ir(inBuf) // WARNING: sound file should be AIFF to allow for floating point sample rates
      val numFrames   = BufFrames.ir(inBuf)
      val phasorRate  = bufRate / SampleRate.ir // = BufRateScale .ir(inBuf)
      val halfPeriod  = numFrames / (bufRate * 2)
      val phasor      = Phasor.ar(speed = phasorRate, lo = diskPad, hi = numFrames - diskPad)

      // ---- clock trigger ----

      // for the trigger, k-rate is sufficient
      val phasorK     = A2K.kr(phasor)
      val interp: GE  = if (diskUsesInterp) (phasorRate - 1.0).signum.abs * 3 + 1 else 1
      val phasorTrig  = Trig1.kr(phasorK - numFrames/2, ControlDur.ir)
      val clockTrig   = phasorTrig + TDelay.kr(phasorTrig, halfPeriod)
      SendTrig.kr(clockTrig, value = PulseCount.kr(clockTrig), id = trigID)

      // ---- actual signal ----

      BufRd.ar(numChannels, buf = inBuf, index = phasor, loop = 0, interp = interp)
    }

    def addAudioVariable(varPlay: Var.Playing): GE = {
      val section0 = sonif.variableMap.getOrElse(Sonification.DefaultVariable,
        sys.error(s"Default variable not specified"))

      withVariable(varPlay.time.range) { (timeName, timeSection) =>
        val timeDim = section0.dimensions.indexWhere(_.name == timeName)
        require (timeDim >= 0, s"Time dimension $timeName is not part of $section0")

        val section = (section0 /: varPlay.variable.operations) {
          case (sect, Var.Select(selection)) =>
            withVariable(selection) { (sectName, sectSect) =>
              require1D(selection, sectSect)
              sect in sectName select sectSect.section.head
            }
          case (sect, op) => sys.error(s"Currently unsupported operation $op for $sect")
        }

        val ctl = ctlNameFromSection(section)
        addAudioMatrix(controlName = ctl, freq = varPlay.time.freq, section = section, streamDim = timeDim)
      }
    }

    // OBSOLETE
    def addMatrixIn(m: MatrixIn): GE = {
      import ugen._
      val key       = m.key
      val source    = getMatrixInSource(m)
      val bufCtl    = bufCtlName(key)
      usedMappings += key
      source match {
        case col @ ColumnSource(_) =>
          val sig = BufRd.kr(numChannels = col.size, buf = bufCtl.ir, index = 0, loop = 1, interp = 0)
          Latch(m.rate, sig)  // instant k- to a-rate

        case _ =>
          val numChannels = source.numRows

          val inBuf       = bufCtl.ir
          val bufRate     = BufSampleRate.ir(inBuf) // WARNING: sound file should be AIFF to allow for floating point sample rates
          val numFrames   = BufFrames    .ir(inBuf)
          val phasorRate  = BufRateScale .ir(inBuf) // = bufRate / SampleRate.ir
          val halfPeriod  = numFrames / (bufRate * 2)
          val phasor      = Phasor(m.rate, speed = phasorRate, lo = diskPad, hi = numFrames - diskPad)

          // ---- clock trigger ----

          // for the trigger, k-rate is sufficient
          val phasorK     = m.rate match {
            case `audio`  => A2K.kr(phasor)
            case `control`=> phasor
            case _        => sys.error("Matrix must run at k- or a-rate")
          }
          val interp: GE  = if (diskUsesInterp) (phasorRate - 1.0).signum.abs * 3 + 1 else 1
          val phasorTrig  = Trig1.kr(phasorK - numFrames/2, ControlDur.ir)
          val clockTrig   = phasorTrig + TDelay.kr(phasorTrig, halfPeriod)
          SendTrig.kr(clockTrig, value = PulseCount.kr(clockTrig), id = 0 /* diskTrigID */)

          // ---- actual signal ----

          BufRd(m.rate, numChannels, buf = inBuf, index = phasor, loop = 0, interp = interp)
      }
    }

    def build(init: SynthGraph): UGenGraphBuilder.Result = {
      val ug = UGenGraph.use(this) {
        var g = init // sonif.graph
        var controlProxies = Set.empty[ControlProxyLike]
        while (g.nonEmpty) {
          controlProxies ++= g.controlProxies
          g = SynthGraph(g.sources.foreach(force))
        }
        build(controlProxies)
      }
      // (ug, usedMappings)
      UGenGraphBuilder.Result(ug, sections, userValues)
    }
  }
}
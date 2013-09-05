package at.iem.sysson
package sound
package impl

import de.sciss.synth
import synth.impl.BasicUGenGraphBuilder
import synth._
import ugen.ControlProxyLike
import at.iem.sysson.graph.SelectedLike

object UGenGraphBuilderImpl {
  def apply(sonif: Sonification, sg: SynthGraph): UGenGraphBuilder.Result = new Impl(sonif).build(sg)

  def bufCtlName (key: String): String = "$son_" + key
//  def rateCtlName(key: String): String = "$son_" + key

  final val diskTrigID      = 0
  final val diskUsesInterp  = false
  final val diskPad         = if (diskUsesInterp) 4 else 0

  private final class Impl(sonif: Sonification) extends BasicUGenGraphBuilder with UGenGraphBuilder {
    import UGenGraphBuilder.Section

    override def toString     = s"UGenGraphBuilder(${sonif.name})@" + hashCode.toHexString

    private var usedMappings  = Set.empty[String]
    private var sections      = Vec.empty[Section]

    def getMatrixInSource(m: MatrixIn): SonificationSource = {
      val key = m.key
      sonif.mapping.getOrElse(key, throw Sonification.MissingInput(key))
    }

    def addScalarSelection(range: SelectedLike): GE =
      range.variable.find(sonif.variableMap)(_._2.variable).fold[GE] {
        sys.error(s"Selection for ${range.variable} not specified")
      } {
        case (name, section) =>
          require(section.rank == 1, s"Selection for ${range.variable} must be one-dimensional")

          val ctl         = section.shape.mkString(s"$$var_${section.name}_", "_", "")
          val uSect       = Section(controlName = ctl, peer = section, stream = -1)
          val numFramesL  = section.size
          require(numFramesL <= 4096, s"Scalar selection too large ($numFramesL > 4096)")
          val numFrames   = numFramesL.toInt
          sections      :+= uSect

          import ugen._
          Index.kr(buf = ctl.ir, in = 0 until numFrames)
      }

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
          val numFrames   = BufFrames.ir(inBuf)
          val phasorRate  = BufRateScale.ir(inBuf) // = bufRate / SampleRate.ir
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
          SendTrig.kr(clockTrig, value = PulseCount.kr(clockTrig), id = diskTrigID)

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
      UGenGraphBuilder.Result(ug, sections)
    }
  }
}
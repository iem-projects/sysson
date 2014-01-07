/*
 *  UGenGraphBuilderImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package sound
package impl

import de.sciss.synth
import synth.impl.BasicUGenGraphBuilder
import synth._
import ugen.ControlProxyLike
import at.iem.sysson.graph
import at.iem.sysson.graph.{VarRef, Var, SelectedLike}
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

    // OBSOLETE
    def getMatrixInSource(m: MatrixIn): SonificationSource = {
      val key = m.key
      sonif.mapping.getOrElse(key, throw Sonification.MissingInput(key))
    }

    private def findVariable[A](variable: VarRef): (String, VariableSection) =
      variable.find(sonif.variableMap)(_._2.variable).getOrElse {
        sys.error(s"Selection for $variable not specified")
      }

    private def require1D(section: VariableSection, variable: Any): Unit =
      require(section.rank == 1, s"Selection for $variable must be one-dimensional")

    private def ctlNameFromSection(section: VariableSection): String =
      section.shape.mkString(s"$$var_${section.name}_", "_", "")

    def addScalarUserValue(value: graph.UserValue): GE = {
      val ctl = s"$$user_${value.key}"
      userValues += UGenGraphBuilder.UserValue(controlName = ctl, peer = value)
      ctl.ir(value.default)
    }

    def addScalarSelection(range: SelectedLike): GE = {
      val (_, section) = findVariable(range.variable)
      require1D(section, range.variable)
      addScalarSection(section)
    }

    private def addScalarSection(section: VariableSection): GE = {
      val ctl         = ctlNameFromSection(section)
      val uSect       = Section(controlName = ctl, peer = section, streamDim = -1, streamID = -1)
      val numFramesL  = section.size
      require(numFramesL <= 4096, s"Scalar selection too large ($numFramesL > 4096)")
      val numFrames   = numFramesL.toInt
      sections      :+= uSect

      import ugen._
      Index.kr(buf = ctl.ir, in = 0 until numFrames) // XXX TODO or should enforce audio-rate?
    }

    def addAudioSelection(range: SelectedLike, freq: GE): GE = {
      val (_, section) = findVariable(range.variable)
      require1D(section, range.variable)

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
      val section       = varSection(varPlay.variable)
      val (timeName, _) = findVariable(varPlay.time.range.variable)
      val timeDim = section.dimensions.indexWhere(_.name == timeName)
      require (timeDim >= 0, s"Time dimension $timeName is not part of $section")

      val ctl = ctlNameFromSection(section)
      addAudioMatrix(controlName = ctl, freq = varPlay.time.freq, section = section, streamDim = timeDim)
    }

    private def varSection(variable: Var): VariableSection = {
      val section0 = sonif.variableMap.getOrElse(Sonification.DefaultVariable,
        sys.error(s"Default variable not specified"))

      val section = (section0 /: variable.operations) {
        case (sect, Var.Select(selection)) =>
          val (sectName, sectSect) = findVariable(selection.variable)
          require1D(sectSect, selection.variable)
          sect in sectName select sectSect.section.head

        case (sect, op) => sys.error(s"Currently unsupported operation $op for $sect")
      }

      section
    }

    private def dimSection(variable: Var, dim: String): VariableSection = {
      val vs        = varSection(variable)
      dimSection(vs, dim)
    }

    private def dimSection(vs: VariableSection, dim: String): VariableSection = {
      val dimIdx    = vs.dimensions.indexWhere(_.name == dim)
      require(dimIdx >= 0, s"Section $vs does not contain dimension $dim")
      val range     = vs.section(dimIdx)
      val section0  = sonif.variableMap.getOrElse(dim, sys.error(s"Dimension $dim not found in variable map"))
      require1D(section0, dim)
      section0.copy(section = Vec(range))
    }

    // XXX TODO: DRY - addAudioVariable
    def addScalarAxis(varPlay: Var.Playing, axis: VarRef): GE = {
      // cf. VariableAxesAssociations.txt

      val section0                = varSection(varPlay.variable)

      val (axisName, _)           = findVariable(axis)
      val (timeName, _)           = findVariable(varPlay.time.range.variable)
      val dims                    = section0.dimensions
      val timeDim                 = dims.indexWhere(_.name == timeName)
      val axisDim0                = dims.indexWhere(_.name == axisName)
      require (timeDim  >= 0, s"Time dimension $timeName is not part of $section0")
      require (axisDim0 >= 0, s"Axis dimension $axisName is not part of $section0")
      require (axisDim0 != timeDim, s"Axis $axisName is used for temporal unrolling")
      val axisDim                 = if (axisDim0 < timeDim) axisDim0 else axisDim0 - 1  // after removing time
      val shapeRed                = section0.shape.patch(timeDim, Vec.empty, 1)
      val div                     = shapeRed.drop(axisDim + 1).product    // XXX TODO guard against overflow
      val axisSize                = shapeRed(axisDim)

      // println(s"Reduced shape: $shapeRed, axisSize $axisSize, div $div")

      val axisSection             = dimSection(varPlay.variable, axisName)
      val axisSignal: GE          = addScalarSection(axisSection)

      Vec.tabulate(axisSize * div)(i => axisSignal \ (i/div))
    }

    ////////////////////////////////////////////

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
  }
}
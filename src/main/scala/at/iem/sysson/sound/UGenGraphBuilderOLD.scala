/*
 *  UGenGraphBuilderOLD.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package sound

import de.sciss.synth.{SynthGraph, GE, UGenGraph}
import at.iem.sysson.graph
import at.iem.sysson.graph.{VarRef, Var, SelectedLike}
import impl.{UGenGraphBuilderImplOLD => Impl}
import at.iem.sysson.legacy.{SonificationSourceOLD, SonificationOLD}

object UGenGraphBuilderOLD {
  /** XXX TODO: need to accommodate reductions such as averaging
    *
    * @param controlName  the control name to use
    * @param peer         the client side variable section
    * @param streamDim    the index of the dimension to stream, of `-1` if using static buffer
    * @param streamID     send-trig identifier for streamed sections
    */
  case class Section(controlName: String, peer: VariableSection, streamDim: Int, streamID: Int) {
    def variable    = peer.variable
    def isStreaming = streamDim >= 0
  }

  case class UserValue(controlName: String, peer: graph.UserValue)

  case class Result(graph: UGenGraph, sections: Vec[Section], userValues: Set[UserValue])

  def apply(sonif: SonificationOLD, sg: SynthGraph): Result = Impl(sonif, sg)
}
trait UGenGraphBuilderOLD extends UGenGraph.Builder {
  def addMatrixIn(m: MatrixIn): GE
  def getMatrixInSource(m: MatrixIn): SonificationSourceOLD

  def addScalarSelection(range: SelectedLike          ): GE
  def addAudioSelection (range: SelectedLike, freq: GE): GE
  def addAudioVariable  (variable: Var.Play           ): GE
  def addScalarUserValue(value: graph.UserValue       ): GE
  def addScalarAxis     (variable: Var.Play, axis: VarRef): GE
}
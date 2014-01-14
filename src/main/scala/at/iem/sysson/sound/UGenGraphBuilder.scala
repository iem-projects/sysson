/*
 *  UGenGraphBuilder.scala
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

import de.sciss.synth.{SynthGraph, GE, UGenGraph}
import at.iem.sysson.graph
import at.iem.sysson.graph.{VarRef, SelectedRange, Var, SelectedLike}
import impl.{UGenGraphBuilderImpl => Impl}

object UGenGraphBuilder {
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
trait UGenGraphBuilder extends UGenGraph.Builder {
  def addMatrixIn(m: MatrixIn): GE
  def getMatrixInSource(m: MatrixIn): SonificationSource

  def addScalarSelection(range: SelectedLike          ): GE
  def addAudioSelection (range: SelectedLike, freq: GE): GE
  def addAudioVariable  (variable: Var.Play           ): GE
  def addScalarUserValue(value: graph.UserValue       ): GE
  def addScalarAxis     (variable: Var.Play, axis: VarRef): GE
}
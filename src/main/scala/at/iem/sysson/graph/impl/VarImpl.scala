/*
 *  VarImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013 Institute of Electronic Music and Acoustics, Graz.
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
package graph
package impl

import de.sciss.synth.{GE, ScalarRated, UGenInLike}
import at.iem.sysson.graph.Var
import at.iem.sysson.sound.UGenGraphBuilder

object VarImpl {
  def Default: Var = Impl(Vec.empty)

  private final case class Impl(operations: Vec[Var.Op]) extends Var {
    private def select1(selection: SelectedLike): Impl = {
      requireUnusedReduction(selection.variable)
      copy(operations = operations :+ Var.Select(selection))
    }

    private def requireUnusedReduction(v: VarRef): Unit =
      require(!operations.exists {
        case r: Var.Reduction if r.variable == v => true
        case _ => false
      }, s"Dimension $v has already been selected or reduced")

    def select(selections: SelectedLike*): Var = (this /: selections)(_ select1 _)

    def average(dim: VarRef): Var = {
      requireUnusedReduction(dim)
      copy(operations = operations :+ Var.Average(dim))
    }

    def ir: Var.GE = ???
    def kr: Var.GE = ???

    def play(time: SelectedRange.Playing): Var.Playing = new PlayingImpl(this, time)
  }

  private final case class AxisImpl(playing: Var.Playing, ref: VarRef) extends Var.Axis {
    def values    : GE = new AxisValuesImpl(this)

    def indices   : GE = ???

    def startValue: GE = ???

    def endValue  : GE = ???
  }

  private final case class AxisValuesImpl(axis: AxisImpl) extends LazyImpl with ScalarRated {
    protected def makeUGens(b: UGenGraphBuilder): UGenInLike =
      b.addScalarAxis(axis.playing, axis.ref)
  }

  private final case class PlayingImpl(variable: Impl, time: SelectedRange.Playing)
    extends LazyImpl with Var.Playing {

    def axis(ref: VarRef): Var.Axis = new AxisImpl(this, ref)

    protected def makeUGens(b: UGenGraphBuilder): UGenInLike =
      b.addAudioVariable(this)
  }
}

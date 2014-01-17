/*
 *  Var.scala
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

package at.iem.sysson.graph

import de.sciss.synth
import de.sciss.synth.{ScalarRated, UGenInLike, AudioRated}
import at.iem.sysson._
import de.sciss.serial.ImmutableSerializer
import at.iem.sysson.sound.UGenGraphBuilder

object Var {
  trait GE extends synth.GE {
    def variable: Var

    def axis(dim: Dim): Axis
  }

  case class Play(variable: Var, time: Dim.Play)
    extends impl.LazyImpl with GE with AudioRated {

    override def productPrefix = "Var$Play"

    def axis(dim: Dim): Var.Axis = {
      require (dim.variable == variable, s"Dimension ${dim.name} does not belong to variable $variable")
      Var.Axis(this, dim.name)
    }

    protected def makeUGens(b: UGenGraphBuilder): UGenInLike = b.addAudioVariable(this)
  }

  // def apply(): Var = impl.VarImpl.Default

  /** Declares a new sonification variable (data source).
    *
    * @param name         Logical name by which the source is referred to
    * @param higherRank   Whether a matrix rank higher than `dimensions.size` is permitted
    */
  def apply(name: String, /* dims: Vec[Dim], */ higherRank: Boolean = false): Var =
    Impl(name, higherRank)

  def unapply(vr: Var): Option[(String, /* Vec[Dim], */ Boolean /*, Vec[Var.Op] */)] = Some(
    vr.name, vr.higherRank
  )

  implicit def serializer: ImmutableSerializer[Var] = impl.VarImpl.serializer

  // ---- axis -----

  // XXX TODO: should be common trait with SelectedRange (values, indices, extent, size, startValue, ...)

  object Axis {
    case class Values(axis: Var.Axis) extends impl.LazyImpl with ScalarRated {
      override def productPrefix = "Var$Axis$Values"

      protected def makeUGens(b: UGenGraphBuilder): UGenInLike =
        ??? // b.addScalarAxis(axis.playing, axis.ref)
    }
  }

  /** A reference to a dimensional axis with respect to a variable section.
    * The difference between this and for instance `SelectDim` is that the
    * graph element producing methods such as `values` and `indices` produce
    * multi-channel signals which correctly align with the underlying variable section.
    * This allows signal processing which combines each sample value from a
    * variable with the corresponding axes elements.
    */
  case class Axis(playing: Var.Play, dim: String) {
    override def productPrefix = "Var$Axis"

    def values    : synth.GE = Axis.Values(this)

    def indices   : synth.GE = ???

    def startValue: synth.GE = ???

    def endValue  : synth.GE = ???
  }

  // ---- operations ----

  sealed trait Op

  sealed trait Reduction extends Op {
    def variable: VarRef
  }

  final case class Select (selection: SelectedLike) extends Reduction {
    override def productPrefix = "Var$Select"

    def variable: VarRef = selection.variable
  }
  final case class Average(variable: VarRef) extends Reduction

  // -------- VarImpl --------

  private final case class Impl(name: String, higherRank: Boolean)
    extends Var {

    override def productPrefix = "Var"



    //    private def select1(selection: SelectedLike): Impl = {
    //      requireUnusedReduction(selection.variable)
    //      copy(operations = operations :+ Var.Select(selection))
    //    }

    //    private def requireUnusedReduction(v: VarRef): Unit =
    //      require(!operations.exists {
    //        case r: Var.Reduction if r.variable == v => true
    //        case _ => false
    //      }, s"Dimension $v has already been selected or reduced")

    //    def select(selections: SelectedLike*): Var = (this /: selections)(_ select1 _)
    //
    //    def average(dim: VarRef): Var = {
    //      requireUnusedReduction(dim)
    //      copy(operations = operations :+ Var.Average(dim))
    //    }

    def dim(name: String): Dim = Dim(this, name)

    def ir: Var.GE = ???
    def kr: Var.GE = ???

    def play(time: Dim.Play): Var.Play = Var.Play(this, time)
  }
}
trait Var extends UserInteraction {
  /** Logical name by which the source is referred to */
  def name: String

  // /** List of specifications of required dimensions */
  // def dims: Vec[Dim]

  def dim(name: String): Dim

  /** Whether a matrix rank higher than `dimensions.size` is permitted */
  def higherRank: Boolean

  //  def select(selections: SelectedLike*): Var
  //  def average(dim: VarRef): Var

  def ir: Var.GE
  def kr: Var.GE

  /** A special sectioning which unrolls one of the variable dimensions in time. */
  def play(time: Dim.Play): Var.Play

  // /** Creates a dimension reference */
  // def dim(dim: Dim): Dim

  //  /** The operations performed on the original input variable, such as taking slices,
  //    * averaging over a dimension, etc.
  //    */
  //  def operations: Vec[Var.Op]
}

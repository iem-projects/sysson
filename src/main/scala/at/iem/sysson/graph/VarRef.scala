/*
 *  VarRef.scala
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

import Implicits._
import ucar.nc2
import de.sciss.synth
import synth.AudioRated

sealed trait VarRef {
  def find[A](vars: Iterable[A])(implicit view: A => nc2.Variable): Option[A] = {
    val n = ncName
    vars.find(view(_).name == n)
  }

  /** Conventional Netcdf name */
  protected def ncName: String
}

object Var {
  trait GE extends synth.GE {
    def variable: Var

    // XXX TODO should be axis().values and axis().indices
    // def axisValues(ref: VarRef): GE

    def axis(ref: VarRef): Axis
  }

  trait Playing extends GE with AudioRated {
    def time: SelectedRange.Playing
  }

  def apply(): Var = impl.VarImpl.Default

  // ---- axis -----

  // XXX TODO: should be common trait with SelectedRange (values, indices, extent, size, startValue, ...)

  /** A reference to a dimensional axis with respect to a variable section.
    * The difference between this and for instance SelectRange is that the
    * graph element producing methods such as `values` and `indices` produce
    * multi-channel signals which correctly align with the underlying variable section.
    * This allows signal processing which combines each sample value from a
    * variable with the corresponding axes elements.
    */
  trait Axis {
    def values    : synth.GE
    def indices   : synth.GE
    def startValue: synth.GE
    def endValue  : synth.GE
  }

  // ---- operations ----

  sealed trait Op

  sealed trait Reduction extends Op {
    def variable: VarRef
  }

  final case class Select (selection: SelectedLike) extends Reduction {
    def variable: VarRef = selection.variable
  }
  final case class Average(variable: VarRef) extends Reduction
}
trait Var {
  def select(selections: SelectedLike*): Var
  def average(dim: VarRef): Var

  def ir: Var.GE
  def kr: Var.GE

  /** A special sectioning which unrolls one of the variable dimensions in time. */
  def play(time: SelectedRange.Playing): Var.Playing

  /** The operations performed on the original input variable, such as taking slices,
    * averaging over a dimension, etc.
    */
  def operations: Vec[Var.Op]
}

case object Time extends VarRef {
  protected val ncName = "time"
}

case object Latitude extends VarRef {
  protected val ncName = "lat"
}

case object Longitude extends VarRef {
  protected val ncName = "lon"
}

case object Pressure extends VarRef {
  protected val ncName = "plev"
}

case object Altitude extends VarRef {
  protected val ncName = "alt"
}
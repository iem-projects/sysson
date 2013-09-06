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
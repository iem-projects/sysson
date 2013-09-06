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
  trait Axis {
    def values : GE
    def indices: GE
    def startValue: GE
    def endValue: GE
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

  def play(time: SelectedRange.Playing): Var.Playing

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
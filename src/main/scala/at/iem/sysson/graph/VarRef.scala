package at.iem.sysson
package graph

import Implicits._
import ucar.nc2
import de.sciss.synth
import synth.{AudioRated, GE, UGenInLike}

sealed trait VarRef {
  def find(vars: Vec[nc2.Variable]): Option[nc2.Variable] = {
    val n = ncName
    vars.find(_.name == n)
  }

  /** Conventional Netcdf name */
  protected def ncName: String
}

object Var {
  case class GE(variable: Var, time: SelectedRange.GE) extends synth.GE with AudioRated {
    def axis(ref: VarRef): GE = ???

    def expand: UGenInLike = ???
  }

  def apply(): Var = ???
}
trait Var {
  def select(selections: SelectedLike*): Var = ???
  def average(dim: VarRef): Var
  def ir: GE = ???
  def ar(time: SelectedRange.GE): Var.GE = Var.GE(this, time)
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
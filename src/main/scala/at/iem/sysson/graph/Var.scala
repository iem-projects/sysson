package at.iem.sysson
package graph

import ucar.nc2
import Implicits._

sealed trait Var {
  def find(vars: Vec[nc2.Variable]): Option[nc2.Variable] = {
    val n = ncName
    vars.find(_.name == n)
  }

  /** Conventional Netcdf name */
  protected def ncName: String
}

case object Time extends Var {
  protected val ncName = "time"
}

case object Latitude extends Var {
  protected val ncName = "lat"
}

case object Longitude extends Var {
  protected val ncName = "lon"
}

case object Pressure extends Var {
  protected val ncName = "plev"
}

case object Altitude extends Var {
  protected val ncName = "alt"
}

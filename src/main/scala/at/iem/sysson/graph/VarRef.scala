/*
 *  VarRef.scala
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
package graph

import Implicits._
import ucar.nc2
import de.sciss.synth
import synth.AudioRated
import de.sciss.serial.ImmutableSerializer

sealed trait VarRef {
  final def apply() = this  // for the current SynthGraph serializer impl

  def find[A](vars: Iterable[A])(implicit view: A => nc2.Variable): Option[A] = {
    val n = ncName
    vars.find(view(_).name == n)
  }

  /** Conventional Netcdf name */
  protected def ncName: String
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
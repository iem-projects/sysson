/*
 *  Polar.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2016 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson.binaural

import scala.math.{sin, cos}

/** Polar (two angles) representation of a 3D point.
  *
  * @param theta aka elevation
  * @param phi   aka azimuth
  */
final case class Polar(theta: Double, phi: Double) {
  def toCartesian: Point3D = {
    val x = sin(theta) * cos(phi)
    val y = sin(theta) * sin(phi)
    val z = cos(theta)
    Point3D(x, y, z)
  }

  override def toString = f"$productPrefix(theta = $theta%1.3f, phi = $phi%1.3f)"
}

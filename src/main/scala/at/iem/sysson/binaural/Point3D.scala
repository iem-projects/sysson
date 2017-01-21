/*
 *  Point3D.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson.binaural

import scala.math.{acos, atan2, sqrt}

final case class Point3D(x: Double, y: Double, z: Double) {
  def distanceTo(that: Point3D): Double = {
    val dx = that.x - this.x
    val dy = that.y - this.y
    val dz = that.z - this.z
    sqrt(dx * dx + dy * dy + dz * dz)
  }

  def toPolar: Polar = Polar(acos(z), atan2(y, x))

  override def toString = f"$productPrefix(x = $x%1.3f, y = $y%1.3f, z = $z%1.3f)"
}

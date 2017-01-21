/*
 *  Point2D.scala
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

import scala.math.sqrt

final case class Point2D(x: Double, y: Double) {
  def distanceTo(that: Point2D): Double = {
    val dx = that.x - this.x
    val dy = that.y - this.y
    sqrt(dx * dx + dy * dy)
  }

  def angleTo(that: Point2D): Radians = {
    val r = math.atan2(-(that.y - this.y), that.x - this.x)
    Radians(r)
  }

  override def toString = f"$productPrefix(x = $x%1.3f, y = $y%1.3f)"
}

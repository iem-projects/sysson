/*
 *  Radians.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson.binaural

import de.sciss.numbers.Implicits._

object Radians {
  val North = Radians(math.Pi * 0.5)
}
final case class Radians(value: Double) extends AnyVal {
  /** Wraps the value to ensure it lies within -Pi ... +Pi */
  def normalize: Radians = Radians(value.wrap2(math.Pi))

  def - (that: Radians): Radians = Radians(this.value - that.value)
  def + (that: Radians): Radians = Radians(this.value + that.value)

  def angleTo(that: Radians): Radians = {
    val thisN = this.normalize
    val thatN = that.normalize
    (thatN - thisN).normalize
  }
}

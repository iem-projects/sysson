/*
 *  OpenRange.scala
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

import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import language.implicitConversions

object OpenRange {
  private final val COOKIE = 0x4F52

  implicit def closed(r: Range): OpenRange =
    new OpenRange(startOption = Some(r.start), endOption = Some(r.end), isInclusive = r.isInclusive, step = r.step)

  final val all = new OpenRange(None, None, isInclusive = true)

//  def at(idx: Int) = apply(Some(idx), Some(idx), isInclusive = true)
  object at {
    def apply(idx: Int) = new OpenRange(Some(idx), Some(idx), isInclusive = true)
  }

  implicit object Serializer extends ImmutableSerializer[OpenRange] {
    def read(in: DataInput): OpenRange = {
      val cookie = in readShort()
      require (cookie == COOKIE, s"Unexpected cookie (expected ${COOKIE.toHexString}, found ${cookie.toInt.toHexString})")
      val opt = ImmutableSerializer.option[Int]
      val startOption = opt read in
      val endOption   = opt read in
      val isInclusive = in readBoolean()
      val step        = in readInt()
      OpenRange(startOption = startOption, endOption = endOption, isInclusive = isInclusive, step = step)
    }

    def write(r: OpenRange, out: DataOutput): Unit = {
      import r._
      val opt = ImmutableSerializer.option[Int]
      out writeShort   COOKIE
      opt write       (startOption, out)
      opt write       (endOption  , out)
      out writeBoolean isInclusive
      out writeInt     step
    }
  }
}
final case class OpenRange(startOption: Option[Int], endOption: Option[Int], isInclusive: Boolean, step: Int = 1) {
  def by(step: Int) = copy(step = step)

  def isAll: Boolean = startOption.isEmpty && stopOption.isEmpty

  /**
   * The exclusive stop position, taking care of the `isInclusive` setting
   */
  def stopOption = if (isInclusive) endOption.map(_ + 1) else endOption

  override def toString: String = {
    (startOption, endOption) match {
      case (Some(start), Some(end)) if start == end && isInclusive => s"at $start"
      case (None, None) => "all"
      case _ => genericToString
    }
  }

  private def genericToString: String = {
    val startS  = startOption.getOrElse("start")
    val endS    = endOption.getOrElse("end")
    val moveS   = if (isInclusive) "to" else "until"
    val byS     = if (step == 1) "" else s" by $step"
    s"($startS $moveS ${endS}$byS)"
  }

  /** Note: `maxStop` is always _exclusive_ */
  def toClosedRange(minStart: Int, maxStop: Int): Range = {
    val start = math.max(minStart, startOption.getOrElse(minStart))
    val end   = if (isInclusive) {
      math.min(maxStop - 1, endOption.getOrElse(maxStop - 1))
    } else {
      math.min(maxStop, endOption.getOrElse(maxStop))
    }
    (if (isInclusive) start to end else start until end) by step
  }
}
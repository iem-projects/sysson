/*
 *  OpenRange.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson

object OpenRange {
  implicit def closed(r: Range): OpenRange =
    new OpenRange(startOption = Some(r.start), endOption = Some(r.end), isInclusive = r.isInclusive, step = r.step)

  final val all = new OpenRange(None, None, isInclusive = true)

//  def at(idx: Int) = apply(Some(idx), Some(idx), isInclusive = true)
  object at {
    def apply(idx: Int) = new OpenRange(Some(idx), Some(idx), isInclusive = true)
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
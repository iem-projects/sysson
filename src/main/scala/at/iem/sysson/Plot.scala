/*
 *  Plot.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson

import at.iem.sysson.impl.{PlotImpl => Impl}
import de.sciss.lucre.event.Publisher
import de.sciss.lucre.expr.StringObj
import de.sciss.lucre.matrix.Matrix
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, Serializer}

object Plot extends Obj.Type {
  final val typeID = 0x30006

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)

  // ---- implementation forwards ----

  def apply[S <: Sys[S]](matrix: Matrix[S])(implicit tx: S#Tx): Plot[S] = Impl[S](matrix)

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Plot[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Plot[S]] = Impl.serializer[S]

  final case class Update[S <: Sys[S]](plot: Plot[S], changes: Vec[Change[S]])
  sealed trait Change[S <: Sys[S]]
  final case class MatrixChange[S <: Sys[S]](peer: Matrix.Update[S]) extends Change[S]
  final case class DimsChange  [S <: Sys[S]](peer: evt.Map.Update[S, String, StringObj])
    extends Change[S]

  /** Conventional key for dimensions map. Value is string denoting horizontal axis dimension name. */
  final val HKey = "X-Axis"
  /** Conventional key for dimensions map. Value is string denoting vertical axis dimension name. */
  final val VKey = "Y-Axis"

  final val attrShowOverlay = "show-overlay"
  final val attrPalette     = "palette"
}
trait Plot[S <: Sys[S]] extends Obj[S] with Publisher[S, Plot.Update[S]] {
  def matrix: Matrix[S]

  final def tpe: Obj.Type = Plot

  /** Maps axis names to dimension names.
    *
    * By convention the key for the horizontal axis is `Plot.HKey`,
    * the key for the vertical axis is `Plot.VKey`.
    */
  def dims: evt.Map[S, String, StringObj]
}

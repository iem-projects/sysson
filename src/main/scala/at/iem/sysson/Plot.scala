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

import de.sciss.lucre.expr.Expr
import de.sciss.lucre.{expr, event => evt}
import de.sciss.lucre.event.{Publisher, Sys}
import de.sciss.lucre.matrix.Matrix
import de.sciss.model
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.synth.proc
import impl.{PlotImpl => Impl}

object Plot {
  final val typeID = 0x30006

  // ---- implementation forwards ----

  def apply[S <: Sys[S]](matrix: Matrix[S])(implicit tx: S#Tx): Plot[S] = Impl[S](matrix)

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Plot[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: evt.NodeSerializer[S, Plot[S]] = Impl.serializer[S]

  final case class Update[S <: Sys[S]](plot: Plot[S], changes: Vec[Change[S]])
  sealed trait Change[S <: Sys[S]]
  final case class MatrixChange[S <: Sys[S]](peer: Matrix.Update[S]) extends Change[S]
  final case class DimsChange  [S <: Sys[S]](peer: expr.Map.Update[S, String, Expr[S, String], model.Change[String]])
    extends Change[S]

  /** Conventional key for dimensions map. Value is string denoting horizontal axis dimension name. */
  final val HKey = "X-Axis"
  /** Conventional key for dimensions map. Value is string denoting vertical axis dimension name. */
  final val VKey = "Y-Axis"

  final val attrShowOverlay = "show-overlay"
  final val attrPalette     = "palette"

  // ---- element ----

  object Elem {
    def apply[S <: Sys[S]](peer: Plot[S])(implicit tx: S#Tx): Plot.Elem[S] =
      Impl.ElemImpl(peer)

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Plot.Elem[S]] =
      Impl.ElemImpl.serializer
  }
  trait Elem[S <: Sys[S]] extends proc.Elem[S] {
    type Peer       = Plot[S]
    type PeerUpdate = Plot.Update[S]
    type This       = Elem[S]
  }

  object Obj {
    def unapply[S <: Sys[S]](obj: proc.Obj[S]): Option[Plot.Obj[S]] =
      if (obj.elem.isInstanceOf[Plot.Elem[S]]) Some(obj.asInstanceOf[Plot.Obj[S]])
      else None
  }

  type Obj[S <: Sys[S]] = proc.Obj.T[S, Plot.Elem]
}
trait Plot[S <: Sys[S]] extends evt.Node[S] with Publisher[S, Plot.Update[S]] {
  def matrix: Matrix[S]

  /** Maps axis names to dimension names.
    *
    * By convention the key for the horizontal axis is `Plot.HKey`,
    * the key for the vertical axis is `Plot.VKey`.
    */
  def dims: expr.Map[S, String, Expr[S, String], model.Change[String]]

  def mkCopy()(implicit tx: S#Tx): Plot[S]
}

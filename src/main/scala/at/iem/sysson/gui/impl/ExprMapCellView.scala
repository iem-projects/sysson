package at.iem.sysson.gui
package impl

import de.sciss.lucre.event.Sys
import de.sciss.lucre.{stm, expr}
import de.sciss.lucre.expr.{Expr, ExprType}
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.swing.CellView
import de.sciss.lucre.swing.impl.CellViewImpl
import de.sciss.model.Change
import de.sciss.serial.Serializer

object ExprMapCellView {
  def apply[S <: Sys[S], K, A](map: expr.Map[S, K, Expr[S, A], Change[A]], key: K)
                              (implicit tx: S#Tx, keySerializer: Serializer[S#Tx, S#Acc, K],
                               tpe: ExprType[A]): CellView[S#Tx, Option[A]] = {
    import tpe.{serializer => valueSerializer}
    implicit val ser = expr.Map.serializer[S, K, Expr[S, A], Change[A]]
    new Impl(tx.newHandle(map), key)
  }

  private final class Impl[S <: Sys[S], K, A](mapH: stm.Source[S#Tx, expr.Map[S, K, Expr[S, A], Change[A]]], key: K)
    extends CellView[S#Tx, Option[A]] with CellViewImpl.Basic[S#Tx, Option[A]] {

    type Repr = Option[Expr[S, A]]

    def repr(implicit tx: S#Tx): Repr = mapH().get(key)

    def react(fun: S#Tx => Option[A] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = mapH().changed.react {
      implicit tx => upd => upd.changes.foreach {
        case expr.Map.Added   (`key`, ex)               => fun(tx)(Some(ex.value))
        case expr.Map.Removed(`key`, _)                 => fun(tx)(None)
        case expr.Map.Element(`key`, _, Change(_, now)) => fun(tx)(Some(now))
        case _ =>
      }
    }

    def apply()(implicit tx: S#Tx): Option[A] = repr.map(_.value)
  }
}
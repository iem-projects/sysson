/*
 *  EditExprVar.scala
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

package at.iem.sysson.gui.edit

import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm
import de.sciss.lucre.expr.Expr
import javax.swing.undo.{UndoableEdit, AbstractUndoableEdit}
import de.sciss.serial

object EditExprVar {
  def apply[S <: Sys[S], A](name: String, expr: Expr.Var[S, A], value: Expr[S, A])
                           (implicit tx: S#Tx, cursor: stm.Cursor[S],
                            serializer   : serial.Serializer[S#Tx, S#Acc, Expr    [S, A]],
                            varSerializer: serial.Serializer[S#Tx, S#Acc, Expr.Var[S, A]]): UndoableEdit = {

    val exprH   = tx.newHandle(expr)
    val beforeH = tx.newHandle(expr())
    val nowH    = tx.newHandle(value)
    val res     = new Impl(name, exprH, beforeH, nowH)
    res.perform()
    res
  }

  private final class Impl[S <: Sys[S], A](name: String,
                                           exprH  : stm.Source[S#Tx, Expr.Var [S, A]],
                                           beforeH: stm.Source[S#Tx, Expr     [S, A]],
                                           nowH   : stm.Source[S#Tx, Expr     [S, A]])(implicit cursor: stm.Cursor[S])
    extends AbstractUndoableEdit {

    override def undo(): Unit = {
      super.undo()
      cursor.step { implicit tx =>
        val expr  = exprH()
        expr()    = beforeH()
      }
    }

    override def redo(): Unit = {
      super.redo()
      cursor.step { implicit tx => perform() }
    }

    def perform()(implicit tx: S#Tx): Unit = {
      val expr  = exprH()
      expr()    = nowH()
    }

    override def getPresentationName = name
  }
}

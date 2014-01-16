/*
 *  EditExprVar.scala
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

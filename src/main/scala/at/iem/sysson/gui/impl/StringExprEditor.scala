/*
 *  StringExprEditor.scala
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
package gui
package impl

import scala.swing.TextField
import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.stm
import de.sciss.desktop.UndoManager
import scala.swing.event.EditDone
import de.sciss.lucre.expr.{Type, Expr}
import de.sciss.lucre.synth.expr.Strings
import at.iem.sysson.gui.edit.EditExprVar

object StringExprEditor {
  def apply[S <: Sys[S]](expr: Expr[S, String], name: String, columns: Int = 16)
                        (implicit tx: S#Tx, cursor: stm.Cursor[S], undoManager: UndoManager): StringExprEditor[S] = {
    import Strings._
    val exprH     = expr match {
      case Expr.Var(exprV) => Some(tx.newHandle(exprV))
      case _               => None
    }
    val value0    = expr.value
    val res       = new Impl[S](exprH, value0 = value0, editName = name, columns0 = columns)
    res.observer  = expr.changed.react { implicit tx => res.update(_) }

    GUI.fromTx(res.guiInit())
    res
  }

  private final class Impl[S <: Sys[S]](exprH: Option[stm.Source[S#Tx, Expr.Var[S, String]]], value0: String,
                                        editName: String, columns0: Int)
                                       (implicit cursor: stm.Cursor[S], undoManager: UndoManager)
    extends StringExprEditor[S] with ExprEditor[S, String, TextField] {

    protected var value = value0

    var observer: Disposable[S#Tx] = _

    // protected val tpe: Type[String] = Strings

    protected def valueToComponent(): Unit = component.text = value

    // should be called when the GUI component has been edited. this will update `value`,
    // transactionally update the expression (if editable), and register an undoable edit
    protected def commit(newValue: String): Unit = {
      if (value != newValue) {
        exprH.foreach { h =>
          val edit = cursor.step { implicit tx =>
            import Strings.{serializer, varSerializer, newConst}
            EditExprVar[S, String](s"Change $editName", expr = h(), value = newConst(newValue))
          }
          undoManager.add(edit)
        }
        value = newValue
      }
      clearDirty()
    }

    protected def createComponent(): TextField = {
      val txt     = new TextField(value, columns0)
      dirty       = Some(DirtyBorder(txt))

      txt.listenTo(txt)
      txt.reactions += {
        case EditDone(_) => commit(txt.text)
      }
      observeDirty(txt)
      txt
    }
  }
}
trait StringExprEditor[S <: Sys[S]] extends Disposable[S#Tx] {
  def component: TextField
}
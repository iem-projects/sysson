/*
 *  ExprEditor.scala
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

import scala.swing.{TextComponent, Component}
import de.sciss.lucre.stm.Disposable
import at.iem.sysson.gui.edit.EditExprVar
import de.sciss.model.Change
import javax.swing.event.{UndoableEditEvent, UndoableEditListener}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.{Type, Expr}
import de.sciss.lucre.stm
import de.sciss.desktop.UndoManager

//object ExprEditor {
//
//}
trait ExprEditor[S <: Sys[S], A, Comp <: Component] extends ComponentHolder[Comp] {
  // ---- abstract ----

  // the current ephemeral (but committed) value of the view. sub classes should
  // implement this with the correct initial value
  protected var value: A

  // must be implemented by updating the GUI component with the current `value`
  protected def valueToComponent(): Unit

  //  // if the expression is a variable, Some(var), otherwise None
  //  protected def exprH: Option[stm.Source[S#Tx, Expr.Var[S, A]]]

  // protected implicit def cursor: stm.Cursor[S]

  // protected def undoManager: UndoManager

  //  // the type name that is used for undoable edits, e.g. `String`
  //  protected def editName: String

  //  // the expression type system
  //  protected val tpe: Type[A]

  // must be implemented by creating the GUI component
  protected def createComponent(): Comp

  // final var observer: Disposable[S#Tx] = _
  protected def observer: Disposable[S#Tx]

  // maybe be set by the sub class in `createComponent()`
  final protected var dirty = Option.empty[DirtyBorder]

  // clears the dirty status if `dirty` is non empty
  protected def clearDirty(): Unit = dirty.foreach(_.visible = false)

  // called when the expression changes, so that the change will be reflected in the GUI
  final def update(ch: Change[A])(implicit tx: S#Tx): Unit =
    if (value != ch.now) GUI.fromTx {
      value           = ch.now
      valueToComponent()
      clearDirty()
    }

  // installs an edit listener for the given text component which will flag `dirty` upon the first change
  // to the component's document
  final protected def observeDirty(text: TextComponent): Unit =
    text.peer.getDocument.addUndoableEditListener(new UndoableEditListener {
      def undoableEditHappened(e: UndoableEditEvent): Unit = dirty.foreach(_.visible = true)
    })

  final def guiInit(): Unit =
    component = createComponent()

  // disposes the observer.
  def dispose()(implicit tx: S#Tx): Unit = observer.dispose()
}

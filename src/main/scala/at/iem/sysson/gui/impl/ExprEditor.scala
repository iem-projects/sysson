/*
 *  ExprEditor.scala
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
package gui
package impl

import scala.swing.{TextComponent, Component}
import de.sciss.lucre.stm.Disposable
import javax.swing.event.{UndoableEditEvent, UndoableEditListener}
import de.sciss.lucre.event.Sys
import java.awt.event.{KeyEvent, KeyListener, FocusEvent, FocusListener}

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
  final def update(newValue: A)(implicit tx: S#Tx): Unit =
    if (value != newValue) GUI.fromTx {
      value = newValue
      valueToComponent()
      clearDirty()
    }

  // installs an edit listener for the given text component which will flag `dirty` upon the first change
  // to the component's document
  final protected def observeDirty(text: TextComponent): Unit = {
    // the fucking JSpinner implementation removes and re-inserts its text when focused,
    // at least with aqua LaF. this means that two undoable edits are fired which are
    // completely pointless and idiotically marked as "significant". In order to skip
    // them, we register focus and key listeners.
    val j = text.peer
    // var valid = true // false
    j.getDocument.addUndoableEditListener(new UndoableEditListener {
      def undoableEditHappened(e: UndoableEditEvent): Unit = {
        // if (valid) {
          // println(s"UNDOABLE EDIT: ${e.getEdit}")
          dirty.foreach(_.visible = true)
        // }
      }
    })
    //    text.peer.addFocusListener(new FocusListener {
    //      def focusLost  (e: FocusEvent) = ()
    //      def focusGained(e: FocusEvent): Unit = valid = false
    //    })
    //    text.peer.addKeyListener(new KeyListener {
    //      def keyReleased(e: KeyEvent): Unit = ()
    //      def keyPressed (e: KeyEvent): Unit = valid = true
    //      def keyTyped   (e: KeyEvent): Unit = ()
    //    })
  }

  final def guiInit(): Unit =
    component = createComponent()

  // disposes the observer.
  def dispose()(implicit tx: S#Tx): Unit = observer.dispose()
}

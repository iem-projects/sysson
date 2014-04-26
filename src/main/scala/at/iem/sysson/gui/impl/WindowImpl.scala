/*
 *  WindowImpl.scala
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

package at.iem.sysson.gui
package impl

import de.sciss.{pdflitz, desktop}
import scala.swing.{Component, Action}
import de.sciss.lucre.event.Sys
import de.sciss.file._
import de.sciss.synth.Optional
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing._
import javax.swing.JComponent
import java.awt.{Graphics2D, Dimension}
import de.sciss.desktop.KeyStrokes
import scala.swing.event.Key

object WindowImpl {
  final val WindowKey = "at.iem.sysson.Window"

  private final class Peer[S <: Sys[S]](view: View[S], impl: WindowImpl[S],
                                        undoRedoActions: Option[(Action, Action)],
                                        override val style: desktop.Window.Style)
    extends desktop.impl.WindowImpl {

    def handler = SwingApplication.windowHandler

    // bindMenu("window.windowShot", Action(null)(windowShot()))

    addAction("window-shot", new ActionWindowShot(this))

    view match {
      case fv: View.File => file = Some(fv.file)
      case _ =>
    }
    file.map(_.base).foreach(title = _)

    contents  = view.component
    closeOperation = desktop.Window.CloseIgnore
    reactions += {
      case desktop.Window.Closing  (_) => impl.handleClose()
      case desktop.Window.Activated(_) =>
        view match {
          case wv: ViewHasWorkspace[S] =>
            DocumentViewHandler.instance.activeDocument = Some(wv.workspace)
          case _ =>
        }
    }

    bindMenu("file.close", Action(null)(impl.handleClose()))

    undoRedoActions.foreach { case (undo, redo) =>
      bindMenus(
        "edit.undo" -> undo,
        "edit.redo" -> redo
      )
    }

    pack()
  }
}
abstract class WindowImpl[S <: Sys[S]](title0: Optional[String] = None)
  extends Window[S] with ComponentHolder[desktop.Window] {

  impl =>

  protected def style: desktop.Window.Style = desktop.Window.Regular

  final def window: desktop.Window = component
  private var windowImpl: WindowImpl.Peer[S] = _

  final protected def title        : String        = windowImpl.title
  final protected def title_=(value: String): Unit = windowImpl.title = value

  final protected def dirty        : Boolean        = windowImpl.dirty
  final protected def dirty_=(value: Boolean): Unit = windowImpl.dirty = value

  final def init()(implicit tx: S#Tx): Unit = {
    view match {
      case wv: ViewHasWorkspace[S] => wv.workspace.addDependent(impl)
      case _ =>
    }
    deferTx(guiInit())
  }

  // /** Subclasses may override this if they invoke `super.guiInit()` first. */
  private def guiInit(): Unit = {
    val f       = new WindowImpl.Peer(view, impl, undoRedoActions, style)
    title0.foreach(f.title_=)
    component   = f
    windowImpl  = f
    val (ph, pv, pp) = placement
    GUI.placeWindow(f, ph, pv, pp)
    f.front()

    // so that the component may find a "window ancestor"
    view.component.peer.putClientProperty(WindowImpl.WindowKey, f)
  }

  /** Subclasses may override this. The tuple is (horiz, vert, padding) position.
    * By default it centers the window, i.e. `(0.5f, 0.5f, 20)`.
    */
  protected def placement: (Float, Float, Int) = (0.5f, 0.5f, 20)

  /** Subclasses may override this. If this method returns `true`, the window may be closed,
    * otherwise a closure is aborted. By default this always returns `true`.
    */
  protected def checkClose(): Boolean = true

  /** Subclasses may override this. */
  protected def undoRedoActions: Option[(Action, Action)] =
    view match {
      case ev: View.Editable[S] =>
        val mgr = ev.undoManager
        Some(mgr.undoAction -> mgr.redoAction)
      case _ => None
    }

  private var didClose = false
  private def disposeFromGUI(): Unit = if (!didClose) {
    requireEDT()
    view match {
      case cv: View.Cursor[S] => cv.cursor.step { implicit tx =>
          dispose()
        }
      case _ =>
    }
    didClose = true
  }

  final def handleClose(): Unit = {
    requireEDT()
    if (checkClose()) disposeFromGUI()
  }

  def dispose()(implicit tx: S#Tx): Unit = {
    view match {
      case wv: ViewHasWorkspace[S] => wv.workspace.removeDependent(this)
      case _ =>
    }

    view.dispose()
    deferTx {
      window.dispose()
      didClose = true
    }
  }
}
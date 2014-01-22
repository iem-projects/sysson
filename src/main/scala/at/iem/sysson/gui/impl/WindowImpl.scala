package at.iem.sysson.gui
package impl

import de.sciss.desktop
import scala.swing.Action
import de.sciss.lucre.event.Sys
import de.sciss.file._
import de.sciss.synth.Optional

abstract class WindowImpl[S <: Sys[S]](title0: Optional[String] = None)
  extends Window[S] with ComponentHolder[desktop.Window] {

  impl =>

  def window: desktop.Window = component

  def init()(implicit tx: S#Tx): Unit = {
    view match {
      case wv: View.Workspace[S] => wv.workspace.addDependent(impl)
      case _ =>
    }
    GUI.fromTx(guiInit())
  }

  private def guiInit(): Unit = {
    val f: desktop.Window = new desktop.impl.WindowImpl {
      def style   = desktop.Window.Regular
      def handler = SwingApplication.windowHandler

      view match {
        case fv: View.File => file = Some(fv.file)
        case _ =>
      }
      file.map(_.base).orElse(title0).foreach(title = _)

      contents  = view.component
      closeOperation = desktop.Window.CloseIgnore
      reactions += {
        case desktop.Window.Closing  (_) => disposeFromGUI()
        case desktop.Window.Activated(_) =>
          view match {
            case wv: View.Workspace[S] =>
              DocumentViewHandler.instance.activeDocument = Some(wv.workspace)
            case _ =>
          }
      }

      bindMenu("file.close", Action(null)(disposeFromGUI()))

      view match {
        case ev: View.Editable[S] =>
          bindMenus(
            "edit.undo" -> ev.undoManager.undoAction,
            "edit.redo" -> ev.undoManager.redoAction
          )

        case _ =>
      }

      pack()
    }

    component = f
    val (ph, pv, pp) = placement
    GUI.placeWindow(f, ph, pv, pp)
    f.front()
  }

  protected def placement: (Float, Float, Int) = (0.5f, 0.5f, 20)

  private var didClose = false
  private def disposeFromGUI(): Unit = if (!didClose) {
    GUI.requireEDT()
    view match {
      case cv: View.Cursor[S] => cv.cursor.step { implicit tx =>
          dispose()
        }
      case _ =>
    }
    didClose = true
  }

  def dispose()(implicit tx: S#Tx): Unit = {
    view match {
      case wv: View.Workspace[S] => wv.workspace.removeDependent(this)
      case _ =>
    }

    view.dispose()
    GUI.fromTx {
      window.dispose()
      didClose = true
    }
  }
}
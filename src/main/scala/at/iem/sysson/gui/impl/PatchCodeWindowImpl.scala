package at.iem.sysson
package gui
package impl

import de.sciss.desktop.{UndoManager, OptionPane}
import scala.swing.{Component, Action, Swing}
import Swing._
import de.sciss.lucre.stm
import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm.Disposable
import de.sciss.swingplus.Implicits._
import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import de.sciss.syntaxpane.SyntaxDocument

object PatchCodeWindowImpl {
  def apply[S <: Sys[S]](entry: Library.Leaf[S], undoManager: UndoManager)
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): PatchCodeWindow[S] = {
    val view    = PatchCodeView(entry, undoManager)
    val name0   = entry.name.value
    val res     = new Impl[S](view, name0 = name0, contextName = Code.SynthGraph.name) {
      val observer = entry.name.changed.react { implicit tx => ch =>
        GUI.fromTx {
          name = ch.now
        }
      }
    }
    res.init()
    res
  }

  private def mkTitle(name: String, contextName: String) = s"$name : $contextName Code"

  private abstract class Impl[S <: Sys[S]](val view: PatchCodeView[S], name0: String, contextName: String)
                                          (implicit cursor: stm.Cursor[S])
    extends WindowImpl[S](title0 = mkTitle(name0, contextName)) with PatchCodeWindow[S] {

    // import code.{contextName, id => codeID}

    protected def observer: Disposable[S#Tx]

    private var _name = ""

    def name = _name
    def name_=(value: String): Unit = {
      _name = value
      title = mkTitle(name, contextName)
    }

    override def checkClose(): Boolean = {
      if (view.isCompiling) return false

      if (view.dirty) {
        val message = "The code has been edited.\nDo you want to save the changes?"
        val opt = OptionPane.confirmation(message = message, optionType = OptionPane.Options.YesNoCancel,
          messageType = OptionPane.Message.Warning)
        opt.title = "Close Code Editor" // s"Close Code Editor - $name"
        opt.show(Some(component)) match {
          case OptionPane.Result.No  =>
          case OptionPane.Result.Yes =>
            view.save()

          case OptionPane.Result.Cancel | OptionPane.Result.Closed =>
            return false
        }
      }
      true // disposeFromGUI()
    }

    private def disposeFromGUI(): Unit = {
      cursor.step { implicit tx =>
        disposeData()
      }
      component.dispose()
    }

    private def disposeData()(implicit tx: S#Tx): Unit = {
      observer.dispose()
    }

    view.addListener {
      case PatchCodeView.DirtyChange(value) => dirty = value
    }

    override def undoRedoActions = Some(view.undoAction -> view.redoAction)


    //    private class WindowImpl2(top: Component, actionApply: Action) extends WindowImpl {
    //      frame =>
    //
    //      def setDirty(value: Boolean): Unit = {
    //        dirty               = value
    //        actionApply.enabled = value
    //      }
    //      def getDirty = dirty
    //
    //    }
    //  }
  }
}
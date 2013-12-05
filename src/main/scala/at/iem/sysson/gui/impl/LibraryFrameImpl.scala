package at.iem.sysson
package gui
package impl

import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window

object LibraryFrameImpl {
  def apply[S <: Sys[S]](library: Library[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): LibraryFrame[S] = {
    val view  = LibraryView(library)
    val res   = new Impl(view)
    GUI.fromTx(res.guiInit())
    res
  }

  private final class Impl[S <: Sys[S]](val view: LibraryView[S]) extends LibraryFrame[S] {
    def guiInit(): Unit = {
      val undoManager = view.undoManager

      val f = new WindowImpl {
        frame =>

        def style       = Window.Regular
        def handler     = SwingApplication.windowHandler

        title           = "Library"
        contents        = view.component
        closeOperation  = Window.CloseDispose

        // val actionUndo  = undoManager.undoAction

        bindMenus(
          // "file.save" -> saveAction,
          "edit.undo" -> undoManager.undoAction,
          "edit.redo" -> undoManager.redoAction
        )

        //        actionUndo.peer.addPropertyChangeListener(new PropertyChangeListener {
        //          def propertyChange(e: PropertyChangeEvent): Unit = if (e.getPropertyName == "enabled") {
        //            dirty = actionUndo.enabled
        //          }
        //        })

        pack()
        GUI.placeWindow(this, 1f, 0.25f, 20)

        // def setDirtyFlag(value: Boolean): Unit = dirty = value
      }
      f.front()
    }
  }
}

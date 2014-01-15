/*
 *  SonificationFrameImpl.scala
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

import de.sciss.lucre.event.Sys
import at.iem.sysson.sound.Sonification
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window
import scala.swing.Action

object SonificationFrameImpl {
  def apply[S <: Sys[S]](workspace: Workspace[S], sonification: Sonification[S])
                        (implicit tx: S#Tx): SonificationFrame[S] = {
    val view  = SonificationView(workspace, sonification)
    val res   = new Impl(workspace, view)
    workspace.addDependent(res)
    GUI.fromTx(res.guiInit())
    res
  }

  private final class Impl[S <: Sys[S]](val workspace: Workspace[S], val view: SonificationView[S])
    extends SonificationFrame[S] with ComponentHolder[Window] {

    def frame: Window = component

    def guiInit(): Unit = {
      val undoManager = view.undoManager

      val f = new WindowImpl {
        frame =>

        def style       = Window.Regular
        def handler     = SwingApplication.windowHandler

        title           = "Sonification Editor"
        contents        = view.component
        closeOperation = Window.CloseIgnore
        reactions += {
          case Window.Closing(_) => disposeFromGUI()
          case Window.Activated(_) =>
            DocumentViewHandler.instance.activeDocument = Some(workspace)
        }

        bindMenus(
          "file.close" -> Action(null)(disposeFromGUI()),
          "edit.undo"  -> undoManager.undoAction,
          "edit.redo"  -> undoManager.redoAction
        )
        pack()
        GUI.placeWindow(this, 0.5f, 0.5f, 20)
      }
      f.front()
      component = f
    }

    private var didClose = false
    private def disposeFromGUI(): Unit = if (!didClose) {
      GUI.requireEDT()
      workspace.cursor.step { implicit tx =>
        dispose()
      }
      didClose = true
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      workspace.removeDependent(this)
      view.dispose()
      GUI.fromTx {
        frame.dispose()
        didClose = true
      }
    }
  }
}

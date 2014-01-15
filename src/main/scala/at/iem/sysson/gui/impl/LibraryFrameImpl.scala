/*
 *  LibraryFrameImpl.scala
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
        GUI.placeWindow(this, 1f, 0.5f, 20)

        // def setDirtyFlag(value: Boolean): Unit = dirty = value
      }
      f.front()
    }
  }
}

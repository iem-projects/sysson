/*
 *  MenuFactory.scala
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

import java.awt.event.KeyEvent
import java.io.{RandomAccessFile, FilenameFilter}
import scala.util.control.NonFatal
import de.sciss.desktop.{KeyStrokes, Menu}
import scala.swing.Action
import de.sciss.lucre.event.Durable
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.file._
import de.sciss.lucre.stm

object MenuFactory {

  def root: Menu.Root = _root

  private lazy val _root = {
    import Menu._
    import KeyEvent._
    import KeyStrokes._

    val dh = DocumentHandler.instance

    val actionCloseAll = new Action("Close All") {
      accelerator = Some(menu1 + shift + VK_W)
      def apply(): Unit = closeAll()
    }

    def checkCloseAll(): Unit =
      actionCloseAll.enabled = dh.allDocuments.hasNext

    checkCloseAll()

    dh.addListener {
      case DocumentHandler.Opened(doc) =>
        // recent.add(doc.dir)
        checkCloseAll()

      case DocumentHandler.Closed(doc) =>
        checkCloseAll()
    }

    Root().add(
      Group("file", "File").add(
        Group("new", "New").add(
          Item("new-workspace", ActionNewWorkspace) // ("Workspace..." -> (menu1 + VK_N)) {
        ).addLine()
        .add(
          Item("new-interpreter")("Interpreter..." -> (menu1 + VK_R)) {
            openInterpreter()
          }
        ).add(
          Item("new-library")("Library..." -> (menu1 + VK_L)) {
            openLibrary()
          }
        )
      ).add(Item("open", ActionOpenWorkspace))
      .add(ActionOpenWorkspace.recentMenu)
      .addLine().add(
        Item("close", proxy("Close" -> (menu1 + VK_W)))
      ).add(
        Item("close-all", actionCloseAll)
      ).add(
        Item("save", proxy("Save" -> (menu1 + VK_S)))
      )
    ).add(
      Group("edit", "Edit").add(
        Item("undo", proxy("Undo" -> (menu1 + VK_Z)))
      ).add(
        Item("redo", proxy("Redo" -> (menu1 + shift + VK_Z)))
      )
    ).add(
      Group("tools", "Tools")
        .add(
          Item("sonif-declaration")("Sonification Declaration TEST..." -> (menu1 + VK_D)) { sonif.DeclarationEditor() }
        )
      //      .add(
      //        Item("designer")("Sound Designer..." -> (menu1 + VK_D)) { openSoundDesigner() }
      //      )
    ).add(
      Group("view", "View").add(
        Item("clear-log")("Clear Log Window" -> (menu1 + shift + VK_P)) {
          LogWindow.instance.log.clear()
        }
      )
    ).add(
      Group("window", "Window")
    )
  }

  def closeAll(): Unit = ??? // DocumentHandler.instance.allDocuments.foreach(_.close())

  //  def openSoundDesigner(): Unit =
  //    sound.designer.DesignerView()

  def openInterpreter(): Unit = InterpreterView()

  private type S = Durable
  private implicit lazy val system: S = {
    val store = BerkeleyDB.factory(dir = syssonDir / "library")
    Durable(store)
  }

  private lazy val libraryH: stm.Source[S#Tx, Library[S]] =
    system.root { implicit tx =>
      val _lib  = Library[S]
      //      val imp   = ExprImplicits[S]
      //      import imp._
      //      _lib.root.insertLeaf  (0, "Test-Leaf", "Test-Source")
      //      val sub = _lib.root.insertBranch(0, "Test-Branch")
      //      sub.insertLeaf       (0, "Test-Child", "Test-Source")
      _lib
    }

  def openLibrary(): Unit =
    system.step { implicit tx =>
      val lib = libraryH()
      LibraryFrame(lib)
    }
}
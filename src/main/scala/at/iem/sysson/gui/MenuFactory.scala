/*
 *  MenuFactory.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013 Institute of Electronic Music and Acoustics, Graz.
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

import java.awt.FileDialog
import java.awt.event.KeyEvent
import java.io.{RandomAccessFile, File, FilenameFilter}
import scala.util.control.NonFatal
import de.sciss.desktop.{RecentFiles, KeyStrokes, Menu}
import scala.swing.Action

object MenuFactory {

  def root: Menu.Root = _root

  private lazy val _root = {
    import Menu._
    import KeyEvent._
    import KeyStrokes._

    val dh = DocumentHandler.instance

    val recent = RecentFiles(SwingApplication.userPrefs("recent-docs")) { f =>
      dh.openRead(f.getPath)
    }

    val actionCloseAll = new Action("Close All") {
      accelerator = Some(menu1 + shift + VK_W)
      def apply(): Unit = closeAll()
    }

    def checkCloseAll(): Unit =
      actionCloseAll.enabled = dh.allDocuments.hasNext

    checkCloseAll()

    dh.addListener {
      case DocumentHandler.Opened(doc) =>
        recent.add(doc.file)
        checkCloseAll()

      case DocumentHandler.Closed(doc) =>
        checkCloseAll()
    }

    Root().add(
      Group("file", "File").add(
        Group("new", "New").add(
          Item("interpreter")("Interpreter..." -> (menu1 + VK_R)) {
            openInterpreter()
          }
        ).add(
          Item("library")("Library..." -> (menu1 + VK_L)) {
            openLibrary()
          }
        )
      ).add(
        Item("open")("Open..." -> (menu1 + VK_O)) {
          openDialog()
        }
      ).add(
        recent.menu
      ).addLine().add(
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

  def closeAll(): Unit = DocumentHandler.instance.allDocuments.foreach(_.close())

  def openDialog(): Unit = {
    val dlg = new FileDialog(null: java.awt.Frame, "Open NetCDF File", FileDialog.LOAD)
    dlg.setFilenameFilter(new FilenameFilter {
      def accept(dir: File, name: String): Boolean = {
        val f = new File(dir, name)
        try {
          // NOTE: NetcdfFile.canOpen is really crappily written, very slow. Therefore,
          // make a short cut and just check for NetCDF cookies
          // NetcdfFile.canOpen(f.getPath)
          val r = new RandomAccessFile(f, "r")
          try {
            if (f.length() < 4) false else {
              val cookie = r.readInt()
              cookie == 0x43444601 || cookie == 0x43444602
            }
          } finally {
            r.close()
          }
        } catch {
          case NonFatal(_) => false
        }
      }
    })
    dlg.setVisible(true)
    val parent  = dlg.getDirectory
    val name    = dlg.getFile
    if (parent == null || name == null) return

    val f = new File(parent, name)
    DocumentHandler.instance.openRead(f.getPath)
  }

  //  def openSoundDesigner(): Unit =
  //    sound.designer.DesignerView()

  def openInterpreter(): Unit = InterpreterView()

  def openLibrary(): Unit = LibraryView(TestLibrary)
}
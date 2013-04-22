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
      def apply() {
        closeAll()
      }
    }

    def checkCloseAll() {
      actionCloseAll.enabled = dh.allDocuments.hasNext
    }
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
      )
    ).add(
      Group("tools", "Tools").add(
        Item("designer")("Sound Designer..." -> (menu1 + VK_D)) {
          openSoundDesigner()
        }
      )
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

  def closeAll() {
    DocumentHandler.instance.allDocuments.foreach(_.close())
  }

  def openDialog() {
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

  def openSoundDesigner() {
    sound.designer.DesignerView()
  }

  def openInterpreter() {
    InterpreterView()
  }
}
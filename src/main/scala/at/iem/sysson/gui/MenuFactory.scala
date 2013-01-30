package at.iem.sysson
package gui

import javax.swing.KeyStroke
import java.awt.{FileDialog, Toolkit}
import java.awt.event.{InputEvent, KeyEvent}
import java.io.{File, FilenameFilter}
import ucar.nc2.NetcdfFile
import util.control.NonFatal

object MenuFactory {
  private def stroke(code: Int, modifiers: Int): KeyStroke = KeyStroke.getKeyStroke(code, modifiers)

  def root: Menu.Root = _root

  private lazy val _root = {
    val meta  = Toolkit.getDefaultToolkit.getMenuShortcutKeyMask
    val shift = InputEvent.SHIFT_MASK

    import Menu._
    import KeyEvent._
    Root().add(
      Group("file", "File").add(
        Item("open")("Open..." -> stroke(VK_O, meta)) {
          openDialog()
        }
      ).add(
        Group("recent", "Open Recent")
      ).addLine().add(
        Item("close", "Close" -> stroke(VK_W, meta))
      ).add(
        Item("close-all")("Close All" -> stroke(VK_W, meta + shift)) {
          closeAll()
        } disable()
//      ).addLine().add(
//        Item("save", "Save" -> stroke(VK_S, meta))
//      ).add(
//        Item("save-as", "Save As..." -> stroke(VK_S, meta + shift))
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
          NetcdfFile.canOpen(f.getPath) // TODO: this is really crappily written, very slow. Should use our own detection mechanism
        } catch {
          case NonFatal(_) => false // sucky `canOpen` throws EOFException in certain cases
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
}
/*
 *  ActionNewWorkspace.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import swing.{Dialog, Action}
import de.sciss.desktop.{Desktop, FileDialog, KeyStrokes}
import scala.util.control.NonFatal
import de.sciss.file._
import GUI.formatException
import scala.swing.event.Key

object ActionNewWorkspace extends Action("Workspace...") {
  import KeyStrokes._
  accelerator = Some(menu1 + Key.N)

  private def deleteRecursive(f: File): Boolean = {
    if (f.isDirectory) {
      f.listFiles().foreach { f1 =>
        if (!deleteRecursive(f1)) return false
      }
    }
    f.delete()
  }

  private def fullName = "New Workspace"

  def apply(): Unit =
    FileDialog.save(title = s"Location for $fullName").show(None).foreach { folder0 =>
      val folder  = folder0.replaceExt(Workspace.ext)

      if (folder.exists()) {
        if (Dialog.showConfirmation(
          message     = s"File ${folder.path} already exists.\nAre you sure you want to overwrite it?",
          title       = fullName,
          optionType  = Dialog.Options.OkCancel,
          messageType = Dialog.Message.Warning
        ) != Dialog.Result.Ok) return

        if (!deleteRecursive(folder)) {
          Dialog.showMessage(
            message     = s"Unable to delete existing file ${folder.path}",
            title       = fullName,
            messageType = Dialog.Message.Error
          )
          return
        }
      }

      try {
        val doc = Workspace.Durable.empty(folder) // Document.empty(folder)
        if (Desktop.isMac) {
          // on OS X, make the folder appear as an opaque file without extension
          import scala.sys.process._
          Seq("SetFile", "-a", "E", folder.path).!
        }
        ActionOpenWorkspace.openGUI(doc)

      } catch {
        case NonFatal(e) =>
          Dialog.showMessage(
            message     = s"Unabled to create new document ${folder.getPath} \n\n${formatException(e)}",
            title       = fullName,
            messageType = Dialog.Message.Error
          )
      }
    }
}

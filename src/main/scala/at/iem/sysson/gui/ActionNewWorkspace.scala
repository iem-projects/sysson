/*
 *  ActionNewWorkspace.scala
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

import swing.{Dialog, Action}
import java.awt.event.KeyEvent
import de.sciss.desktop.{Desktop, FileDialog, KeyStrokes}
import scala.util.control.NonFatal
import de.sciss.file._
import GUI.formatException

object ActionNewWorkspace extends Action("Workspace...") {
  import KeyStrokes._
  accelerator = Some(menu1 + KeyEvent.VK_N)

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
      val name    = folder0.getName
      val folder  = if (name.toLowerCase.endsWith(Workspace.ext))
        folder0
      else
        new File(folder0.getParentFile, s"$name${Workspace.ext}")

      if (folder.exists()) {
        if (Dialog.showConfirmation(
          message     = s"File ${folder.path} already exists.\nAre you sure you want to overwrite it?",
          title       = fullName,
          optionType  = Dialog.Options.OkCancel,
          messageType = Dialog.Message.Warning
        ) != Dialog.Result.Ok) return

        if (!deleteRecursive(folder)) {
          Dialog.showMessage(
            message     = s"Unable to delete existing file ${folder.getPath}",
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

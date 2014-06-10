///*
// *  ActionOpenWorkspace.scala
// *  (SysSon)
// *
// *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
// *  Written by Hanns Holger Rutz.
// *
// *	This software is published under the GNU General Public License v3+
// *
// *
// *	For further information, please contact Hanns Holger Rutz at
// *	contact@sciss.de
// */
//
//package at.iem.sysson
//package gui
//
//import swing.{Dialog, Action}
//import de.sciss.desktop.{Menu, RecentFiles, FileDialog, KeyStrokes}
//import scala.util.control.NonFatal
//import GUI.formatException
//import de.sciss.lucre.event.Sys
//import de.sciss.file._
//import language.existentials
//import scala.swing.event.Key
//import de.sciss.mellite.Workspace
//
//object ActionOpenWorkspace extends Action("Open...") {
//  import KeyStrokes._
//  import DocumentHandler.Document
//
//  private val _recent = RecentFiles(SwingApplication.userPrefs("recent-docs")) { folder =>
//    perform(folder)
//  }
//
//  accelerator = Some(menu1 + Key.O)
//
//  private def fullTitle = "Open Document"
//
//  /** Registers the document with the recent files menu and the document handler.
//    * Does _not_ open a view directly. This should be done by listening to the document handler.
//    */
//  def openGUI[S <: Sys[S]](doc: Workspace[S]): Unit = {
//    recentFiles.add(doc.folder)
//    doc.cursor.step { implicit tx =>
//      DocumentHandler.instance.addDocument(doc)
//    }
//  }
//
//  def recentFiles: RecentFiles  = _recent
//  def recentMenu : Menu.Group   = _recent.menu
//
//  def apply(): Unit = {
//    val dlg = FileDialog.open(title = fullTitle)
//    // dlg.setFilter { f => f.isDirectory && f.ext.toLowerCase == Workspace.ext }
//    dlg.show(None).foreach { f0 =>
//      val f = if (f0.isDirectory) f0 else f0.parent
//      perform(f)
//    }
//  }
//
//  def perform(folder: File): Unit =
//    DocumentHandler.instance.getDocument(folder).fold(openRead(folder)) { doc0 =>
//      // cf. http://stackoverflow.com/questions/20982681/existential-type-or-type-parameter-bound-failure
//      def screwYou[S <: Sys[S]](doc: Workspace[S]): Unit = DocumentViewHandler.instance.getWindow(doc)
//        .foreach(_.window.front())
//
//      screwYou(doc0.asInstanceOf[Workspace[~] forSome { type ~ <: Sys[~] }])
//    }
//
//  private def openRead(folder: File): Unit =
//    try {
//      val doc = Workspace.Durable.read(folder)
//      openGUI(doc)
//
//    } catch {
//      case NonFatal(e) =>
//        Dialog.showMessage(
//          message     = "Unable to create new document " + folder.getPath + "\n\n" + formatException(e),
//          title       = fullTitle,
//          messageType = Dialog.Message.Error
//        )
//    }
//}
/*
 *  DocumentViewHandler.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import impl.{DocumentViewHandlerImpl => Impl}
import de.sciss.model.Model
import DocumentHandler.Document
import de.sciss.lucre.event.Sys
import de.sciss.mellite.Workspace
import de.sciss.lucre.swing.Window

object DocumentViewHandler {
  type WorkspaceWindow[S <: Sys[S]] = Window[S] // MMM

  type View[S <: Sys[S]] = WorkspaceWindow[S]

  lazy val instance: DocumentViewHandler = Impl.instance

  sealed trait Update
  case class Activated[S <: Sys[S]](doc: Workspace[S]) extends Update
}
trait DocumentViewHandler extends Model[DocumentViewHandler.Update] {
  def getWindow[S <: Sys[S]](doc: Workspace[S]): Option[DocumentViewHandler.View[_]]
  // var activeDocument: Option[Document]
  def activeDocument: Option[Document]
  def activeDocument_=[S <: Sys[S]](doc: Option[Workspace[S]]): Unit
}
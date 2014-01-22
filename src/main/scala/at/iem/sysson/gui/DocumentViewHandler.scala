/*
 *  DocumentViewHandler.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
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

object DocumentViewHandler {
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
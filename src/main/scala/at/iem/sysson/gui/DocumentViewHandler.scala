/*
 *  DocumentViewHandler.scala
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

import impl.{DocumentViewHandlerImpl => Impl}
import de.sciss.model.Model
import DocumentHandler.Document
import de.sciss.lucre.event.Sys

object DocumentViewHandler {
  type View[S <: Sys[S]] = WorkspaceView[S]

  lazy val instance: DocumentViewHandler = Impl.instance // Impl()

  sealed trait Update
  case class Activated[S <: Sys[S]](doc: Workspace[S]) extends Update
}
trait DocumentViewHandler extends Model[DocumentViewHandler.Update] {
  import DocumentViewHandler.View

  def getView[S <: Sys[S]](doc: Workspace[S]): Option[View[_]]
  // var activeDocument: Option[Document]
  def activeDocument: Option[Document]
  def activeDocument_=[S <: Sys[S]](doc: Option[Workspace[S]]): Unit
}
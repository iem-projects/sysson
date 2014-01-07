/*
 *  Workspace.scala
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

import de.sciss.lucre.{event => evt, stm}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.LinkedList
import de.sciss.file.File
import impl.{WorkspaceImpl => Impl}

object Workspace {
  // def empty[S <: Sys[S]]
  object Durable {
    def empty(dir: File): Workspace[evt.Durable] = Impl.emptyDurable(dir)
    def read (dir: File): Workspace[evt.Durable] = Impl.readDurable (dir)
  }
}
sealed trait WorkspaceLike {
  type System <: Sys[System]

  implicit def cursor: stm.Cursor[System]

  def dir: File

  def path: String

  def dataSources(implicit tx: System#Tx): LinkedList.Modifiable[System, DataSource[System], Unit]
}
trait Workspace[S <: Sys[S]] {
  type System = S
  //
  //  implicit def cursor: stm.Cursor[S]
  //
  //  def dir: File
  //
  //  def dataSources(implicit tx: S#Tx): LinkedList.Modifiable[S, DataSource[S], Unit]
}
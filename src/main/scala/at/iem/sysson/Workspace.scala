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
import de.sciss.serial.Serializer
import de.sciss.lucre.stm.IdentifierMap
import ucar.nc2.NetcdfFile
import at.iem.sysson.sound.SonificationSpec

object Workspace {
  /** File name extension (including leading period) */
  final val ext = ".sysson"

  // def empty[S <: Sys[S]]
  object Durable {
    def empty(dir: File): Workspace[evt.Durable] = Impl.emptyDurable(dir)
    def read (dir: File): Workspace[evt.Durable] = Impl.readDurable (dir)
  }
}

/** The workspace type for SysSon. A workspace is usually persisted on hard-disk.
  * It contains a collection of data sources, plots and sonification instances.
  */
sealed trait WorkspaceLike {
  type System <: Sys[System]

  /** The transactional cursor associated with this workspace. Typically this is `Durable`. */
  implicit def cursor: stm.Cursor[System]

  /** The opaque (database) directory associated with the workspace. */
  def dir: File

  /** The name of the workspace, which is its directory base name without extension. */
  def name: String

  /** Convenience method for `dir.path`. */
  def path: String

  def dataSources(implicit tx: System#Tx): LinkedList.Modifiable[System, DataSource[System], Unit]

  def sonifSpecs(implicit tx: System#Tx): LinkedList.Modifiable[System, SonificationSpec, Unit]

  // implicit def dataSourceSerializer: Serializer[System#Tx, System#Acc, DataSource[System]]

  private[sysson] def fileCache: IdentifierMap[System#ID, System#Tx, NetcdfFile]
}
trait Workspace[S <: Sys[S]] extends WorkspaceLike {
  type System = S
  //
  //  implicit def cursor: stm.Cursor[S]
  //
  //  def dir: File
  //
  //  def dataSources(implicit tx: S#Tx): LinkedList.Modifiable[S, DataSource[S], Unit]
}
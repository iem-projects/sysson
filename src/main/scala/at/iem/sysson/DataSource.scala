/*
 *  DataSource.scala
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

import ucar.nc2
import java.io.File
import impl.{DataSourceImpl => Impl}
import de.sciss.serial.{DataInput, Writable, Serializer}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm.Mutable

object DataSource {
  def apply[S <: Sys[S]](path: String)(implicit tx: S#Tx): DataSource[S] = Impl(path)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, DataSource[S]] =
    Impl.serializer[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): DataSource[S] = Impl.read(in, access)

  case class Variable[S <: Sys[S]](source: DataSource[S], name: String) // extends
}
/** A document represents one open data file. */
trait DataSource[S <: Sys[S]] extends Writable {
  /** Path to the document's underlying file (NetCDF). */
  def path: String

  def file: File

  def data(workspace: Workspace[S])(implicit tx: S#Tx): nc2.NetcdfFile
}
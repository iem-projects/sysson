/*
 *  DataSourceImpl.scala
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
package impl

import ucar.nc2
import de.sciss.serial.{Serializer, DataInput, DataOutput}
import de.sciss.lucre.{event => evt}
import evt.Sys
import ucar.nc2.NetcdfFile
import scala.concurrent.stm.Txn
import de.sciss.file._

object DataSourceImpl {
  private final val SOURCE_COOKIE = 0x737973736F6E6400L   // "syssond\0"

  def apply[S <: Sys[S]](path: String)(implicit tx: S#Tx): DataSource[S] = new Impl(path)

  private def resolveFile[S <: Sys[S]](workspace: Workspace[S], file: File)(implicit tx: S#Tx): nc2.NetcdfFile =
    workspace.fileCache.get(file)(tx.peer).getOrElse {
      val net = nc2.NetcdfFile.open(file.path).setImmutable()
      workspace.fileCache.put(file, net)(tx.peer)
      Txn.afterRollback { _ =>
        net.close() // a bit tricky doing I/O inside a transaction...
      } (tx.peer)
      net
    }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, DataSource[S]] =
    anySer.asInstanceOf[Ser[S]]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): DataSource[S] = {
    val cookie = in.readLong()
    require(cookie == SOURCE_COOKIE,
      s"Unexpected cookie (found ${cookie.toHexString}, expected ${SOURCE_COOKIE.toHexString})")
    val path  = in.readUTF()
    new Impl(path) // instantiate(id, path)
  }

  private val anySer = new Ser[evt.InMemory]

  private class Ser[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, DataSource[S]] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): DataSource[S] = DataSourceImpl.read(in, access)

    def write(source: DataSource[S], out: DataOutput): Unit = source.write(out)
  }

  private final class Impl[S <: Sys[S]](val path: String)
    extends DataSource[S] {

    override def toString = s"DataSource($path)"

    def file = new File(path)

    def write(out: DataOutput): Unit = {
      out.writeLong(SOURCE_COOKIE)
      out.writeUTF(path)
    }

    def data(workspace: Workspace[S])(implicit tx: S#Tx): NetcdfFile = resolveFile(workspace, file)
  }
}
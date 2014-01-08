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
import java.io.File
import de.sciss.serial.{Serializer, DataInput, DataOutput}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm.{MutableSerializer, Mutable}
import scala.concurrent.stm.Txn

object DataSourceImpl {
  private final val SOURCE_COOKIE = 0x737973736F6E6430L   // "syssond0"

  def apply[S <: Sys[S]](path: String)(implicit workspace: Workspace[S], tx: S#Tx): DataSource[S] = {
    val id = tx.newID()
    instantiate(id, path)
  }

  private def instantiate[S <: Sys[S]](id: S#ID, path: String)
                                      (implicit workspace: Workspace[S], tx: S#Tx): DataSource[S] = {
    import workspace.fileCache
    val data  = fileCache.getOrElse(id, {
      val f = nc2.NetcdfFile.open(path).setImmutable()
      fileCache.put(id, f)
      tx.afterCommit()
      Txn.afterRollback { _ =>
        f.close() // a bit tricky doing I/O inside a transaction...
      } (tx.peer)
      f
    })
    import Implicits._
    new Impl(id, path, data, data.variableMap)
  }

  implicit def serializer[S <: Sys[S]](implicit workspace: Workspace[S]): Serializer[S#Tx, S#Acc, DataSource[S]] =
    new Ser[S]

  private class Ser[S <: Sys[S]](implicit workspace: Workspace[S]) extends MutableSerializer[S, DataSource[S]] {
    protected def readData(in: DataInput, id: S#ID)(implicit tx: S#Tx): DataSource[S] = {
      val cookie = in.readLong()
      require(cookie == SOURCE_COOKIE,
        s"Unexpected cookie (found ${cookie.toHexString}, expected ${SOURCE_COOKIE.toHexString})")
      val path  = in.readUTF()
      instantiate(id, path)
    }
  }

  private final class Impl[S <: Sys[S]](val id: S#ID, val path: String, val data: nc2.NetcdfFile,
                                        val variableMap: Map[String, nc2.Variable])
    extends DataSource[S] with Mutable.Impl[S] {

    override def toString() = s"DataSource(${data.getTitle})"

    def file = new File(path)

    //    def close(): Unit = {
    //      data.close()
    //      dispatch(DataSource.Closed(this))
    //    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeLong(SOURCE_COOKIE)
      out.writeUTF(path)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = ()
  }
}
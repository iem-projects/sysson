/*
 *  WorkspaceImpl.scala
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

import de.sciss.lucre.{event => evt}
import evt.Sys
import de.sciss.serial.{DataOutput, DataInput, Serializer}
import de.sciss.lucre.stm
import java.io.{FileNotFoundException, IOException}
import de.sciss.file._
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.expr.LinkedList
import ucar.nc2
import nc2.NetcdfFile

object WorkspaceImpl {
  def readDurable(dir: File): Workspace[evt.Durable] = {
    if (!dir.isDirectory) throw new FileNotFoundException(s"Document ${dir.path} does not exist")
    applyDurable(dir, create = false)
  }

  def emptyDurable(dir: File): Workspace[evt.Durable] = {
    if (dir.exists()) throw new IOException(s"Document ${dir.path} already exists")
    applyDurable(dir, create = true)
  }

  private def applyDurable(dir: File, create: Boolean): Workspace[evt.Durable] = {
    type S    = evt.Durable
    val fact  = BerkeleyDB.factory(dir, createIfNecessary = create)
    implicit val system: S = evt.Durable(fact)

    //    val access = system.root[Data[S]] { implicit tx =>
    //      val dataSource = LinkedList.Modifiable[S, DataSource]
    //      new Data[S](dataSource)
    //    }

    // implicit val cfTpe  = reflect.runtime.universe.typeOf[S]
    new Impl[S](dir, system, /* access, */ system)
  }

  //  private implicit def dataSer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Data[S]] = new DataSer[S]
  //
  //  private class DataSer[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Data[S]] {
  //    def write(data: Data[S], out: DataOutput): Unit = data.write(out)
  //
  //    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Data[S] = {
  //      val cookie = in.readLong()
  //      require(cookie == WORKSPACE_COOKIE,
  //        s"Unexpected cookie (found ${cookie.toHexString}, expected ${WORKSPACE_COOKIE.toHexString})")
  //      val dataSource = LinkedList.Modifiable.read[S, DataSource](in, access)
  //      new Data(dataSource)
  //    }
  //  }

  private final val WORKSPACE_COOKIE  = 0x737973736F6E7730L   // "syssonw0"

  private final class Data[S <: Sys[S]](val dataSource: LinkedList.Modifiable[S, DataSource[S], Unit]) {
    def write(out: DataOutput): Unit = {
      out.writeLong(WORKSPACE_COOKIE)
      dataSource.write(out)
    }
  }

  private final class Impl[S <: Sys[S]](val dir: File, val system: S,
                                        val cursor: stm.Cursor[S]) extends Workspace[S] {

    def path: String  = dir.path
    def name: String  = dir.base

    implicit def workspace: Workspace[S] = this

    override def toString = s"Workspace($name)"

    val fileCache = cursor.step { implicit tx =>
      tx.newInMemoryIDMap[NetcdfFile]
    }

    private implicit object DataSer extends Serializer[S#Tx, S#Acc, Data[S]] {
      def write(data: Data[S], out: DataOutput): Unit = data.write(out)

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Data[S] = {
        val cookie = in.readLong()
        require(cookie == WORKSPACE_COOKIE,
          s"Unexpected cookie (found ${cookie.toHexString}, expected ${WORKSPACE_COOKIE.toHexString})")
        val dataSource = LinkedList.Modifiable.read[S, DataSource[S]](in, access)
        new Data(dataSource)
      }
    }

    private val data: stm.Source[S#Tx, Data[S]] = system.root { implicit tx =>
      val dataSource = LinkedList.Modifiable[S, DataSource[S]]
      new Data[S](dataSource)
    }

    def dataSources(implicit tx: S#Tx): LinkedList.Modifiable[S, DataSource[S], Unit] = data().dataSource
  }
}

///*
// *  DocumentImpl.scala
// *  (SysSon)
// *
// *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
// *  Written by Hanns Holger Rutz.
// *
// *	This software is free software; you can redistribute it and/or
// *	modify it under the terms of the GNU General Public License
// *	as published by the Free Software Foundation; either
// *	version 2, june 1991 of the License, or (at your option) any later version.
// *
// *	This software is distributed in the hope that it will be useful,
// *	but WITHOUT ANY WARRANTY; without even the implied warranty of
// *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// *	General Public License for more details.
// *
// *	You should have received a copy of the GNU General Public
// *	License (gpl.txt) along with this software; if not, write to the Free Software
// *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
// *
// *
// *	For further information, please contact Hanns Holger Rutz at
// *	contact@sciss.de
// */
//
//package at.iem.sysson
//package impl
//
//import ucar.nc2
//import de.sciss.model.impl.ModelImpl
//import java.io.File
//import de.sciss.serial.{Serializer, DataInput, DataOutput, ImmutableSerializer}
//import de.sciss.lucre.event.{InMemory, Sys}
//
//object DataSourceImpl {
//  import Implicits._
//
//  def openRead(path: String): DataSource = {
//    val f   = nc2.NetcdfFile.open(path)
//    val vm  = f.variableMap // build that once
//    new Impl(path, f, vm)
//  }
//
//  private final val SOURCE_COOKIE = 0x737973736F6E6430L  // "syssond0"\
//
//  def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, DataSource] = anySer.asInstanceOf[Ser[S]]
//
//  private val anySer = new Ser[InMemory]
//
//  private final class Ser[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, DataSource] {
//    def write(ds: DataSource, out: DataOutput): Unit = {
//      out.writeLong(SOURCE_COOKIE)
//      out.writeUTF(ds.path)
//    }
//
//    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): DataSource = {
//      val cookie = in.readLong()
//      require(cookie == SOURCE_COOKIE,
//        s"Unexpected cookie (found ${cookie.toHexString}, expected ${SOURCE_COOKIE.toHexString})")
//      val path = in.readUTF()
//      ???
//    }
//  }
//
//  private final class Impl(val path: String, val data: nc2.NetcdfFile, val variableMap: Map[String, nc2.Variable])
//    extends DataSource with ModelImpl[DataSource.Update] {
//
//    override def toString = "DataSource(" + data.getTitle + ")"
//
//    def file = new File(path)
//
//    def close(): Unit = {
//      data.close()
//      dispatch(DataSource.Closed(this))
//    }
//  }
//}
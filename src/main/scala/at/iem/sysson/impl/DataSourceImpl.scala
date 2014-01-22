/*
 *  DataSourceImpl.scala
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
package impl

import ucar.nc2
import de.sciss.serial.{Serializer, DataInput, DataOutput}
import de.sciss.lucre.{event => evt}
import evt.Sys
import scala.concurrent.stm.Txn
import de.sciss.file._
import DataSource.Variable

object DataSourceImpl {
  private final val SOURCE_COOKIE = 0x737973736F6E6400L   // "syssond\0"

  private final val VAR_COOKIE    = 0x76617200            // "var\0"

  def apply[S <: Sys[S]](path: String)(implicit tx: S#Tx): DataSource[S] = new Impl(path)

  def variable[S <: Sys[S]](source: DataSource[S], name: String)(implicit tx: S#Tx): Variable[S] =
    new VariableImpl(source, name)

  def writeVariable[S <: Sys[S]](v: Variable[S], out: DataOutput): Unit = {
    out.writeInt(VAR_COOKIE)
    v.source.write(out)
    out.writeUTF(v.name)
  }

  def readVariable[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Variable[S] = {
    val cookie = in.readInt()
    require(cookie == VAR_COOKIE,
      s"Unexpected cookie (found ${cookie.toHexString}, expected ${VAR_COOKIE.toHexString})")
    val source  = DataSource.read(in, access)
    val name    = in.readUTF()
    Variable(source, name)
  }

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

  implicit def varSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Variable[S]] =
    anyVarSer.asInstanceOf[VarSer[S]]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): DataSource[S] = {
    val cookie = in.readLong()
    require(cookie == SOURCE_COOKIE,
      s"Unexpected cookie (found ${cookie.toHexString}, expected ${SOURCE_COOKIE.toHexString})")
    val path  = in.readUTF()
    new Impl(path) // instantiate(id, path)
  }

  private val anySer    = new Ser   [evt.InMemory]

  private val anyVarSer = new VarSer[evt.InMemory]

  private class Ser[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, DataSource[S]] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): DataSource[S] = DataSourceImpl.read(in, access)

    def write(source: DataSource[S], out: DataOutput): Unit = source.write(out)
  }

  private class VarSer[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Variable[S]] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Variable[S] = DataSourceImpl.readVariable(in, access)

    def write(v: Variable[S], out: DataOutput): Unit = DataSourceImpl.writeVariable(v, out)
  }

  private final case class VariableImpl[S <: Sys[S]](source: DataSource[S], name: String) extends Variable[S] {
    def write(out: DataOutput): Unit = writeVariable(this, out)

    def data(workspace: Workspace[S])(implicit tx: S#Tx): nc2.Variable = {
      import at.iem.sysson.Implicits._
      val net = source.data(workspace)
      net.variableMap.getOrElse(name, sys.error(s"Variable '$name' does not exist in data source ${source.file.base}"))
    }
  }

  private final class Impl[S <: Sys[S]](val path: String)
    extends DataSource[S] {

    override def toString = s"DataSource($path)"

    def file = new File(path)

    def write(out: DataOutput): Unit = {
      out.writeLong(SOURCE_COOKIE)
      out.writeUTF(path)
    }

    def data(workspace: Workspace[S])(implicit tx: S#Tx): nc2.NetcdfFile = resolveFile(workspace, file)
  }
}
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
import de.sciss.serial.{ImmutableSerializer, Serializer, DataInput, DataOutput}
import de.sciss.lucre.{event => evt}
import evt.Sys
import de.sciss.file._
import DataSource.Variable
import de.sciss.lucre.stm.Mutable
import at.iem.sysson.Implicits._
import scala.collection.breakOut
import scala.concurrent.stm.Txn

object DataSourceImpl {
  private final val SOURCE_COOKIE = 0x737973736F6E6400L   // "syssond\0"

  private final val VAR_COOKIE    = 0x76617200            // "var\0"

  def apply[S <: Sys[S]](file: File)(implicit tx: S#Tx, workspace: Workspace[S]): DataSource[S] = {
    val netFile = resolveFile(workspace, file)
    val f0      = file

    new Impl[S] {
      ds =>

      val id                = tx.newID()
      val file              = f0
      protected val varRef  = tx.newVar[List[Variable[S]]](id, netFile.variables.map(variable(ds, _))(breakOut))
    }
  }

  def variable[S <: Sys[S]](source: DataSource[S], net: nc2.Variable)(implicit tx: S#Tx): Variable[S] =
    ??? // new VariableImpl(source, parents, name)

  def readVariable[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Variable[S] = {
    val id        = tx.readID(in, access)
    val cookie    = in.readInt()
    require(cookie == VAR_COOKIE,
      s"Unexpected cookie (found ${cookie.toHexString}, expected ${VAR_COOKIE.toHexString})")
    // val source  = DataSource.read(in, access)
    val sourceRef = tx.readVar[DataSource[S]](id, in)
    val parents   = parentsSer.read(in)
    val name      = in.readUTF()
    val shape     = shapeSer.read(in)
    new VariableImpl(sourceRef, parents, name, shape)
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
    val id      = tx.readID(in, access)
    val cookie  = in.readLong()
    require(cookie == SOURCE_COOKIE,
      s"Unexpected cookie (found ${cookie.toHexString}, expected ${SOURCE_COOKIE.toHexString})")
    val path    = in.readUTF()
    val varRef  = tx.readVar[List[Variable[S]]](id, in)
  ???
  }

  private val anySer    = new Ser   [evt.InMemory]

  private val anyVarSer = new VarSer[evt.InMemory]

  private class Ser[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, DataSource[S]] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): DataSource[S] = DataSourceImpl.read(in, access)

    def write(source: DataSource[S], out: DataOutput): Unit = source.write(out)
  }

  private class VarSer[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Variable[S]] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Variable[S] = DataSourceImpl.readVariable(in, access)

    def write(v: Variable[S], out: DataOutput): Unit = v.write(out)
  }

  private val parentsSer  = ImmutableSerializer.list[String]
  import Serializers.RangeSerializer
  private val shapeSer    = ImmutableSerializer.indexedSeq[(String, Range)]

  private final class VariableImpl[S <: Sys[S]](sourceRef: S#Var[DataSource[S]], val parents: List[String],
                                                val name: String, val shape: Vec[(String, Range)])
    extends Variable[S] {

    def source(implicit tx: S#Tx): DataSource[S] = sourceRef()

    def write(out: DataOutput): Unit = {
      out       .writeInt(VAR_COOKIE)
      sourceRef .write(out)
      parentsSer.write(parents, out)
      shapeSer  .write(shape  , out)
      out       .writeUTF(name)
    }

    def rank: Int = shape.size

    lazy val size: Long = {
      var res = 0L
      shape.foreach { tup => res += tup._2.size }
      res
    }

    def data(workspace: Workspace[S])(implicit tx: S#Tx): nc2.Variable = {
      import at.iem.sysson.Implicits._
      val net = source.data(workspace)
      net.variableMap.getOrElse(name, sys.error(s"Variable '$name' does not exist in data source ${source.file.base}"))
    }
  }

  private abstract class Impl[S <: Sys[S]]
    extends DataSource[S] with Mutable.Impl[S] {

    protected def varRef: S#Var[List[Variable[S]]]

    override def toString = s"DataSource($path)"

    // def file = new File(path)

    def path: String = file.path

    protected def writeData(out: DataOutput): Unit = {
      out.writeLong(SOURCE_COOKIE)
      out.writeUTF(path)
      varRef.write(out)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      varRef.dispose()
    }

    def variables(implicit tx: S#Tx): List[Variable[S]] = varRef()

    def data(workspace: Workspace[S])(implicit tx: S#Tx): nc2.NetcdfFile = resolveFile(workspace, file)
  }
}
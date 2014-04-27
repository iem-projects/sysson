/*
 *  WorkspaceImpl.scala
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

import de.sciss.lucre.{event => evt, synth, stm}
import evt.Sys
import de.sciss.serial.{DataOutput, DataInput, Serializer}
import java.io.{FileNotFoundException, IOException}
import de.sciss.file._
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.expr.List
import ucar.nc2
import at.iem.sysson.sound.Sonification
import scala.concurrent.stm.{Ref => STMRef, Txn, TMap}
import de.sciss.lucre.stm.Disposable
import scala.util.control.NonFatal
import de.sciss.synth.proc
import de.sciss.lucre.matrix.DataSource
import de.sciss.synth.proc.Obj

object WorkspaceImpl {
  def readDurable(dir: File): Workspace[proc.Durable] = {
    if (!dir.isDirectory) throw new FileNotFoundException(s"Document ${dir.path} does not exist")
    applyDurable(dir, create = false)
  }

  def emptyDurable(dir: File): Workspace[proc.Durable] = {
    if (dir.exists()) throw new IOException(s"Document ${dir.path} already exists")
    applyDurable(dir, create = true)
  }

  private def applyDurable(dir: File, create: Boolean): Workspace[proc.Durable] = {
    type S    = proc .Durable
    type I    = synth.InMemory
    val fact  = BerkeleyDB.factory(dir, createIfNecessary = create)
    implicit val system: S = proc.Durable(fact)
    implicit val iSys  : I = system.inMemory

    new Impl[S, I](dir, system, iSys, system)
  }

  private final val WORKSPACE_COOKIE  = 0x737973736F6E7700L   // "syssonw\0"

  private final class Data[S <: Sys[S]](val dataSources  : List.Modifiable[S, DataSource  [S], Unit],
                                        val sonifications: List.Modifiable[S, Obj.T[S, Sonification.Elem], Obj.UpdateT[S, Sonification.Elem[S]]]) {
    def write(out: DataOutput): Unit = {
      out.writeLong(WORKSPACE_COOKIE)
      dataSources  .write(out)
      sonifications.write(out)
    }
  }

  private final class Impl[S <: Sys[S], I1 <: synth.Sys[I1]](val file: File, val system: S,
                                                             val inMemorySys: I1,
                                                             val cursor: stm.Cursor[S])
                                                            (implicit val inMemoryTx: S#Tx => I1#Tx,
                                                                      val inMemoryCursor: stm.Cursor[I1])
    extends Workspace[S] {

    type I = I1

    def path: String  = file.path
    def name: String  = file.base

    implicit def workspace: Workspace[S] = this

    override def toString = s"Workspace($name)"

    private val fileCache   = TMap.empty[File, nc2.NetcdfFile]
    private val dependents  = STMRef(Vec.empty[Disposable[S#Tx]])

    private implicit object DataSer extends Serializer[S#Tx, S#Acc, Data[S]] {
      def write(data: Data[S], out: DataOutput): Unit = data.write(out)

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Data[S] = {
        val cookie = in.readLong()
        require(cookie == WORKSPACE_COOKIE,
          s"Unexpected cookie (found ${cookie.toHexString}, expected ${WORKSPACE_COOKIE.toHexString})")
        val dataSources   = List.Modifiable.read[S, DataSource  [S]](in, access)
        // XXX TODO: this is why the Obj.T approach fails :-(
        val sonifications = List.Modifiable.read[S, Obj[S], Obj.Update[S]](in, access)
          .asInstanceOf[List.Modifiable[S, Obj.T[S, Sonification.Elem], Obj.UpdateT[S, Sonification.Elem[S]]]]
        new Data(dataSources, sonifications)
      }
    }

    private val data: stm.Source[S#Tx, Data[S]] = system.root { implicit tx =>
      val dataSources   = List.Modifiable[S, DataSource  [S]]
      // XXX TODO: this is why the Obj.T approach fails :-(
      val sonifications = List.Modifiable[S, Obj[S], Obj.Update[S]]
        .asInstanceOf[List.Modifiable[S, Obj.T[S, Sonification.Elem], Obj.UpdateT[S, Sonification.Elem[S]]]]
      new Data[S](dataSources, sonifications)
    }

    def dataSources  (implicit tx: S#Tx): List.Modifiable[S, DataSource  [S], Unit] =
      data().dataSources

    def sonifications(implicit tx: S#Tx): List.Modifiable[S, Obj.T[S, Sonification.Elem], Obj.UpdateT[S, Sonification.Elem[S]]] =
      data().sonifications

    def resolve(file: File)(implicit tx: S#Tx): nc2.NetcdfFile =
      fileCache.get(file)(tx.peer).getOrElse {
        val net = nc2.NetcdfFile.open(file.path).setImmutable()
        fileCache.put(file, net)(tx.peer)
        Txn.afterRollback { _ =>
          net.close() // a bit tricky doing I/O inside a transaction...
        } (tx.peer)
        net
      }

    def addDependent   (dep: Disposable[S#Tx])(implicit tx: S#Tx): Unit =
      dependents.transform(_ :+ dep)(tx.peer)

    def removeDependent(dep: Disposable[S#Tx])(implicit tx: S#Tx): Unit =
      dependents.transform { in =>
        val idx = in.indexOf(dep)
        require(idx >= 0, s"Dependent $dep was not registered")
        in.patch(idx, Nil, 1)
      } (tx.peer)

    def dispose()(implicit tx: S#Tx): Unit = {
      logInfoTx(s"Dispose workspace $name")
      // first dispose all dependents
      val deps = dependents.get(tx.peer)
      deps.foreach(_.dispose())
      dependents.update(Vec.empty)(tx.peer)
      // grap the file cache entries
      val nets = fileCache.snapshot /* (tx.peer) */.valuesIterator
      // clear the file cache map
      fileCache.retain((_, _) => false)(tx.peer)  // direct method instead of `clear()`
      // if the transaction is successful...
      Txn.afterCommit { _ =>
        // ...actually close the files
        nets.foreach { net =>
          try {
            net.close()
          } catch {
            case NonFatal(e) => e.printStackTrace()
          }
        }

        // ...and close the database
        system.close()
      } (tx.peer)
    }
  }
}

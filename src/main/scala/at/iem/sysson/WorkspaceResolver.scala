/*
 *  WorkspaceResolver.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson

import de.sciss.lucre.event.Sys
import de.sciss.lucre.matrix.DataSource
import ucar.nc2.NetcdfFile
import scala.concurrent.stm.{Txn, InTxn, TMap}
import de.sciss.file._
import de.sciss.lucre.stm.Disposable
import scala.util.control.NonFatal
import de.sciss.mellite.Workspace

/** Associates a workspace with a file cache for NetCDF resources. */
object WorkspaceResolver {
  implicit def apply[S <: Sys[S]](implicit ws: Workspace[S]): DataSource.Resolver[S] = new Wrap(ws)

  // key = workspace
  private val map = TMap.empty[Any, Resolver]

  private final class Wrap[S <: Sys[S]](val ws: Workspace[S]) extends DataSource.Resolver[S] {
    def resolve(file: File)(implicit tx: S#Tx): NetcdfFile = {
      implicit val itx = tx.peer
      val res = map.get(ws).getOrElse {
        val res0 = new ResolverImpl(ws)
        ws.addDependent(res0)
        map.put(ws, res0)
        res0
      }

      res.resolve(file)
    }
  }

  private sealed trait Resolver {
    def resolve(file: File)(implicit tx: InTxn): NetcdfFile
  }

  private final class ResolverImpl[S <: Sys[S]](ws: Workspace[S]) extends Resolver with Disposable[S#Tx] {
    private val fileCache = TMap.empty[File, NetcdfFile]

    def resolve(file: File)(implicit tx: InTxn): NetcdfFile =
      fileCache.get(file).getOrElse {
        val net = NetcdfFile.open(file.path).setImmutable()
        fileCache.put(file, net)
        Txn.afterRollback { _ =>
          net.close() // a bit tricky doing I/O inside a transaction...
        }
        net
      }

    def dispose()(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      map.remove(ws)

      val nets = fileCache.snapshot.valuesIterator
      // clear the file cache map
      fileCache.retain((_, _) => false)   // direct method instead of `clear()`
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
      }
    }
  }
}

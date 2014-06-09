/*
 *  Workspace.scala
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

import de.sciss.lucre.{synth, stm}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.List
import de.sciss.file.File
import impl.{WorkspaceImpl => Impl}
import de.sciss.lucre.stm.Disposable
import ucar.nc2.NetcdfFile
import at.iem.sysson.sound.Sonification
import scala.concurrent.stm.TMap
import de.sciss.synth.proc
import de.sciss.lucre.matrix.DataSource
import de.sciss.synth.proc.Obj

object Workspace {
  /** File name extension (excluding leading period) */
  final val ext = "sysson"

  // def empty[S <: Sys[S]]
  object Durable {
    def empty(dir: File): Workspace[proc.Durable] = Impl.emptyDurable(dir)
    def read (dir: File): Workspace[proc.Durable] = Impl.readDurable (dir)
  }
}

/** The workspace type for SysSon. A workspace is usually persisted on hard-disk.
  * It contains a collection of data sources, plots and sonification instances.
  */
trait Workspace[S <: Sys[S]] extends Disposable[S#Tx] /* with DataSource.Resolver[S] */ {
  /** In-Memory back end system */
  type I <: synth.Sys[I] // with stm.Cursor[I]

  /** The transactional cursor associated with this workspace. Typically this is `Durable`. */
  implicit def cursor: stm.Cursor[S]

  // implicit def inMemoryTx(tx: S#Tx): I#Tx
  implicit def inMemoryTx: S#Tx => I#Tx

  def inMemorySys: I
  implicit def inMemoryCursor: stm.Cursor[I]

  /** The opaque (database) directory associated with the workspace. */
  def file: File

  /** The name of the workspace, which is its directory base name without extension. */
  def name: String

  /** Convenience method for `dir.path`. */
  def path: String

  def dataSources  (implicit tx: S#Tx): List.Modifiable[S, DataSource  [S], Unit]
  def sonifications(implicit tx: S#Tx): List.Modifiable[S, Obj.T[S, Sonification.Elem], Obj.UpdateT[S, Sonification.Elem[S]]]

  /** Adds a dependent which is disposed just before the workspace is disposed.
    *
    * @param dep  the dependent. This must be an _ephemeral_ object.
    */
  private[sysson] def addDependent   (dep: Disposable[S#Tx])(implicit tx: S#Tx): Unit
  private[sysson] def removeDependent(dep: Disposable[S#Tx])(implicit tx: S#Tx): Unit
}
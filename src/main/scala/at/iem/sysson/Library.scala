/*
 *  Library.scala
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

//import de.sciss.lucre.{event => evt}
//import de.sciss.lucre.event.{Publisher, Sys}
//import de.sciss.lucre.expr.Expr
//import at.iem.sysson.impl.{LibraryImpl => Impl}
//import de.sciss.lucre.stm
//import de.sciss.serial.Writable
//import de.sciss.serial
import scala.collection.mutable
//import de.sciss.model.Change
import scala.concurrent.{blocking, Future}
import de.sciss.synth.SynthGraph

object Library {
//  def apply[S <: Sys[S]](implicit tx: S#Tx): Library[S] = Impl[S]
//
//  object NodeLike {
//    implicit def serializer[S <: Sys[S]]: evt.Serializer[S, NodeLike[S]] = Impl.nodeSerializer[S]
//  }
//  sealed trait NodeLike[S <: Sys[S]]
//    extends stm.Mutable[S#ID, S#Tx] with Publisher[S, TreeLike.NodeUpdate[S, Library[S]]]{
//
//    def name: Expr[S, String]
//
//    private[sysson] def toEither: TreeLike.Node[Branch[S], Leaf[S]]
//  }
//
//  sealed trait LeafChange
//  case class Renamed      (change: Change[String]) extends LeafChange // with BranchChange
//  case class SourceChanged(change: Change[String]) extends LeafChange
//
//  type LeafUpdate = Vec[LeafChange]
//
//  type Update       [S <: Sys[S]] = TreeLike.Update      [S, Library[S]]
//  type BranchChange [S <: Sys[S]] = TreeLike.BranchChange[S, Library[S]]
//  type LeafChanged  [S <: Sys[S]] = TreeLike.LeafChanged [S, Library[S]]
//  type BranchUpdate [S <: Sys[S]] = TreeLike.BranchUpdate[S, Library[S]]
//
//  object Branch {
//    implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Branch[S]] = Impl.branchSerializer[S]
//
//    def apply[S <: Sys[S]](name: Expr[S, String])(implicit tx: S#Tx): Branch[S] = Impl.newBranch(name)
//  }
//  trait Branch[S <: Sys[S]]
//    extends TreeLike.Branch[S, Library[S]] with NodeLike[S] with Publisher[S, TreeLike.BranchUpdate[S, Library[S]]] {
//
//    def insert  (idx: Int, node: NodeLike[S])(implicit tx: S#Tx): Unit
//    def removeAt(idx: Int)(implicit tx: S#Tx): Unit
//    def remove  (node: NodeLike[S])(implicit tx: S#Tx): Unit
//  }
//  object Leaf {
//    implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Leaf[S]] = Impl.leafSerializer[S]
//
//    def apply[S <: Sys[S]](name: Expr[S, String], source: Expr[S, String])(implicit tx: S#Tx): Leaf[S] =
//      Impl.newLeaf(name, source)
//
//    // case class Value(name: String, source: String)
//  }
//  trait Leaf[S <: Sys[S]] extends NodeLike[S] {
//    def name  : Expr.Var[S, String]
//    def source: Expr.Var[S, String]
//  }
//
//  implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Library[S]] = Impl.serializer[S]
//
  // ---- compilation ----
  private val sync    = new AnyRef
  private val codeMap = new mutable.WeakHashMap[String, SynthGraph]

  def compile(source: String): Future[SynthGraph] = sync.synchronized(codeMap.get(source)).fold {
    Code.future {
      val graph = blocking { Code.SynthGraph(source).execute(()) }
      sync.synchronized(codeMap.put(source, graph))
      graph
    }

  } (Future.successful(_))
}
//trait Library[S <: Sys[S]]
//  extends TreeLike[S, Library[S]] with stm.Mutable[S#ID, S#Tx]
//  with Writable {
//
//  type BU     = Library.Renamed
//  type LU     = Library.LeafUpdate
//  type Leaf   = Library.Leaf  [S]
//  type Branch = Library.Branch[S]
//}

package at.iem.sysson

import de.sciss.lucre.event.EventLike
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.data
import at.iem.sysson.Voodoo.TreeLike.Update

object Voodoo {
  val Node              = Either
  type Node[+B, +L]     = Either[B, L]
  val IsBranch          = Left
  type IsBranch[+B, +L] = Left[B, L]
  val IsLeaf            = Right
  type IsLeaf[+B, +L]   = Right[B, L]

  trait TreeCompanion[T, L, B] {
    type Update[S <: Sys[S], Elem, Upd] = TreeLike.Update[S, Elem, Upd, T, L, B]
    val Update = TreeLike.Update
  }

  object TreeLike {
    trait BranchLike[S <: Sys[S], Elem, B, L] {
      type N = Node[B, L]

      def size    (implicit tx: S#Tx): Int
      def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, N]
      def isEmpty (implicit tx: S#Tx): Boolean
      def nonEmpty(implicit tx: S#Tx): Boolean
      def apply(idx: Int)(implicit tx: S#Tx): N
      def indexOf(node: N)(implicit tx: S#Tx): Int
      // def indexOfNode(node: N)(implicit tx: S#Tx): Int

      // def changed: EventLike[S, Branch.Update[S, Elem, Upd]]
    }

    trait LeafLike[Elem] {
      def value: Elem
    }

    case class Update[S <: Sys[S], Elem, Upd, T, B, L](tree: T, branch: BranchUpdate[S, Upd, B, L])

    sealed trait NodeUpdate[S <: Sys[S], Upd, B, L]

    case class BranchUpdate[S <: Sys[S], Upd, B, L](branch: B, changes: Vec[BranchChange[S, Upd, B, L]])
      extends NodeUpdate[S, Upd, B, L]

    case class LeafUpdate[S <: Sys[S], Upd, B, L](leaf: L, change: Upd)
      extends NodeUpdate[S, Upd, B, L]

    sealed trait BranchChange[S <: Sys[S], Upd, B, L] {
      def idx: Int
    }
    case class Inserted[S <: Sys[S], Upd, B, L](idx: Int, node: Node[L, B])
      extends BranchChange[S, Upd, B, L]

    case class Removed[S <: Sys[S], Upd, B, L](idx: Int, node: Node[L, B])
      extends BranchChange[S, Upd, B, L]

    case class NodeChange[S <: Sys[S], Upd, B, L](idx: Int, update: NodeUpdate[S, Upd, L, B])
      extends BranchChange[S, Upd, B, L]

    sealed trait Change[S <: Sys[S], Elem]
  }
  trait TreeLike[S <: Sys[S], Elem, Upd, Repr /* <: TreeLike[S, Elem, Repr] */] {
    type Leaf <: TreeLike.LeafLike[Elem]

    type Branch <: TreeLike.BranchLike[S, Elem, Branch, Leaf]

    def root: Branch

    def changed: EventLike[S, TreeLike.Update[S, Elem, Upd, Repr, Branch, Leaf]]
  }

  def test[S <: Sys[S], Elem, Upd, T <: TreeLike[S, Elem, Upd, T]](tree: T)(implicit tx: S#Tx): Unit = {
    tree.root.iterator.foreach {
      case IsLeaf(l) => println(l.value)
      case _ =>
    }
  }

  object SubTree {

  }
  class SubTree[S <: Sys[S], Elem, Upd] extends TreeLike[S, Elem, Upd, SubTree[S, Elem, Upd]] {
    def changed: EventLike[S, Update[S, Elem, Upd, SubTree[S, Elem, Upd], Branch, Leaf]] = ???

    def root: Branch = new Branch {
      def children(implicit tx: S#Tx): data.Iterator[S#Tx, Node[Leaf, Branch]] = ???

      val name = "root"

      def size(implicit tx: S#Tx): Int = ???

      def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, N] = ???

      def isEmpty(implicit tx: S#Tx): Boolean = ???

      def nonEmpty(implicit tx: S#Tx): Boolean = ???

      def apply(idx: Int)(implicit tx: S#Tx): N = ???

      def indexOf(node: N)(implicit tx: S#Tx): Int = ???

      // def indexOfNode(node: N)(implicit tx: S#Tx): Int = ???
    }

    trait Leaf extends TreeLike.LeafLike[Elem] {
      def name: String
    }

    trait Branch extends TreeLike.BranchLike[S, Elem, Branch, Leaf] {
      val name: String
    }
  }

  def test2[S <: Sys[S], Elem, Upd](tree: SubTree[S, Elem, Upd])(implicit tx: S#Tx): Unit = {
    tree.root.iterator.foreach {
      case Left(l) => println(l.name)
      case _ =>
    }
  }
}

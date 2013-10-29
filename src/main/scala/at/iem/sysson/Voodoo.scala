package at.iem.sysson

import de.sciss.lucre.event.EventLike
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.data
import at.iem.sysson.Voodoo.TreeLike.Update

object Voodoo {
  object TreeLike {
    trait BranchLike[S <: Sys[S], L, B] {
      def children(implicit tx: S#Tx): data.Iterator[S#Tx, Either[L, B]] // Node]
    }

    trait LeafLike[Elem] {
      def value: Elem
    }

    case class Update[S <: Sys[S], Elem, Repr](tree: Repr, changes: Vec[Change[S, Elem]])

    sealed trait Change[S <: Sys[S], Elem]
  }
  trait TreeLike[S <: Sys[S], Elem, Repr /* <: TreeLike[S, Elem, Repr] */] {
    type Leaf <: TreeLike.LeafLike[Elem]

    type Branch <: TreeLike.BranchLike[S, Leaf, Branch]

    def root: Branch

    def changed: EventLike[S, TreeLike.Update[S, Elem, Repr]]
  }

  def test[S <: Sys[S], Elem, T <: TreeLike[S, Elem, T]](tree: T)(implicit tx: S#Tx): Unit = {
    tree.root.children.foreach {
      case Left(l) => println(l.value)
      case _ =>
    }
  }

  object SubTree {

  }
  class SubTree[S <: Sys[S], Elem] extends TreeLike[S, Elem, SubTree[S, Elem]] {
    def changed: EventLike[S, Update[S, Elem, SubTree[S, Elem]]] = ???

    def root: Branch = new Branch {
      def children(implicit tx: S#Tx): data.Iterator[S#Tx, Either[Leaf, Branch]] = ???
      val name = "foo"
    }

    trait Leaf extends TreeLike.LeafLike[Elem] {
      def name: String
    }

    trait Branch extends TreeLike.BranchLike[S, Leaf, Branch] {
      val name: String
    }
  }

  def test2[S <: Sys[S], Elem](tree: SubTree[S, Elem])(implicit tx: S#Tx): Unit = {
    tree.root.children.foreach {
      case Left(l) => println(l.name)
      case _ =>
    }
  }
}

package at.iem.sysson

import de.sciss.lucre.{event => evt, data, stm}
import evt.EventLike
import de.sciss.lucre.synth.Sys
import impl.{TreeImpl => Impl}

object Tree {
  case class Update[S <: Sys[S], Elem, Upd](tree: Tree[S, Elem, Upd], changes: Vec[Change[S, Elem, Upd]])

  sealed trait Change[S <: Sys[S], Elem, Upd]

  object Node {
    sealed trait Update[S <: Sys[S], Elem, Upd] extends Tree.Change[S, Elem, Upd] {
      def node: Node[S, Elem, Upd]
    }

    sealed trait Modifiable[S <: Sys[S], Elem, Upd] extends Node[S, Elem, Upd]
  }
  sealed trait Node[S <: Sys[S], Elem, Upd]

  object Branch {
    case class Update[S <: Sys[S], Elem, Upd](branch: Branch[S, Elem, Upd], changes: Vec[Branch.Change[S, Elem, Upd]])
      extends Node.Update[S, Elem, Upd] {

      def node = branch
    }

    sealed trait Change[S <: Sys[S], Elem, Upd]
    case class Inserted[S <: Sys[S], Elem, Upd](idx: Int, node: Node[S, Elem, Upd])
    case class Removed [S <: Sys[S], Elem, Upd](idx: Int, node: Node[S, Elem, Upd])
    case class Modified[S <: Sys[S], Elem, Upd](idx: Int, node: Node.Update[S, Elem, Upd])

    trait Modifiable[S <: Sys[S], Elem, Upd] extends Branch[S, Elem, Upd] with Node.Modifiable[S, Elem, Upd] {
      type NM = Node.Modifiable[S, Elem, Upd]

      def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, NM]

      def append            (elem: Elem)(implicit tx: S#Tx): Leaf[S, Elem, Upd]
      def prepend           (elem: Elem)(implicit tx: S#Tx): Leaf[S, Elem, Upd]
      def insert  (idx: Int, elem: Elem)(implicit tx: S#Tx): Leaf[S, Elem, Upd]

      def appendBranch ()       (implicit tx: S#Tx): Branch.Modifiable[S, Elem, Upd]
      def prependBranch()       (implicit tx: S#Tx): Branch.Modifiable[S, Elem, Upd]
      def insertBranch(idx: Int)(implicit tx: S#Tx): Branch.Modifiable[S, Elem, Upd]

      def removeAt(idx: Int)(implicit tx: S#Tx): NM
      def remove(elem: Elem)(implicit tx: S#Tx): Boolean

      // def removeBranch ()(implicit tx: S#Tx): Branch.Modifiable[S, Elem, Upd]
    }
  }
  trait Branch[S <: Sys[S], Elem, Upd] extends Node[S, Elem, Upd] {
    type N = Node[S, Elem, Upd]

    def size    (implicit tx: S#Tx): Int
    def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, N]
    def isEmpty (implicit tx: S#Tx): Boolean
    def nonEmpty(implicit tx: S#Tx): Boolean
    def apply(idx: Int)(implicit tx: S#Tx): N
    def indexOf(elem: Elem)(implicit tx: S#Tx): Int
    def indexOfNode(node: N)(implicit tx: S#Tx): Boolean

    def changed: EventLike[S, Branch.Update[S, Elem, Upd]]
  }

  object Leaf {
    case class Update[S <: Sys[S], Elem, Upd](leaf: Leaf[S, Elem, Upd], elem: Upd)
      extends Node.Update[S, Elem, Upd] {

      def node = leaf
    }

    //    def apply[S <: Sys[S], Elem](implicit tx: S#Tx): Leaf[S, Elem, Unit] = ???
    //
    //    def apply[S <: Sys[S], Elem, Upd](eventView: Elem => EventLike[S, Upd])
    //                                     (implicit tx: S#Tx): Leaf[S, Elem, Upd] = ???
  }
  trait Leaf[S <: Sys[S], Elem, Upd] extends Node.Modifiable[S, Elem, Upd] {
    def value: Elem
    def changed: EventLike[S, Leaf.Update[S, Elem, Upd]]
  }

  object Modifiable {
    def apply[S <: Sys[S], Elem](implicit tx: S#Tx): Modifiable[S, Elem, Unit] = ???

    def apply[S <: Sys[S], Elem, Upd](eventView: Elem => EventLike[S, Upd])
                                     (implicit tx: S#Tx): Modifiable[S, Elem, Upd] = ???
  }
  trait Modifiable[S <: Sys[S], Elem, Upd] extends Tree[S, Elem, Upd] {
    def root: Tree.Branch.Modifiable[S, Elem, Upd]
  }
}
trait Tree[S <: Sys[S], Elem, Upd] extends stm.Mutable[S#ID, S#Tx] {
  def root: Tree.Branch[S, Elem, Upd]

  def changed: EventLike[S, Tree.Update[S, Elem, Upd]]
}
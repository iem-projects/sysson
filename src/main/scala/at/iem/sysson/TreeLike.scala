package at.iem.sysson

import de.sciss.lucre.event.EventLike
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.data

trait TreeTypes {
  type Node[+B, +L]     = Either[B, L]
  val IsBranch          = Left
  type IsBranch[+B, +L] = Left[B, L]
  val IsLeaf            = Right
  type IsLeaf[+B, +L]   = Right[B, L]
}

object TreeLike extends TreeTypes {
  trait Branch[S <: Sys[S], B, L] {
    def size    (implicit tx: S#Tx): Int
    def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, Node[B, L]]
    def isEmpty (implicit tx: S#Tx): Boolean
    def nonEmpty(implicit tx: S#Tx): Boolean
    def apply(idx: Int)(implicit tx: S#Tx): Node[B, L]
    def indexOf(node: Node[B, L])(implicit tx: S#Tx): Int
  }

  //  trait ModifiableBranch[S <: Sys[S], B, L] extends Branch[S, B, L] {
  //    def append            (elem: Elem)(implicit tx: S#Tx): Leaf[S, Elem, Upd]
  //    def prepend           (elem: Elem)(implicit tx: S#Tx): Leaf[S, Elem, Upd]
  //    def insert  (idx: Int, elem: Elem)(implicit tx: S#Tx): Leaf[S, Elem, Upd]
  //
  //    def appendBranch ()       (implicit tx: S#Tx): Branch.Modifiable[S, Elem, Upd]
  //    def prependBranch()       (implicit tx: S#Tx): Branch.Modifiable[S, Elem, Upd]
  //    def insertBranch(idx: Int)(implicit tx: S#Tx): Branch.Modifiable[S, Elem, Upd]
  //
  //    def removeAt(idx: Int)(implicit tx: S#Tx): NM
  //    def remove(elem: Elem)(implicit tx: S#Tx): Boolean
  //  }

  case class Update[S <: Sys[S], BU, LU, T, B, L](tree: T, branch: BranchUpdate[S, BU, LU, B, L])

  sealed trait NodeUpdate[S <: Sys[S], BU, LU, B, L]

  case class BranchUpdate[S <: Sys[S], BU, LU, B, L](branch: B, changes: Vec[BranchChange[S, BU, LU, B, L]])
    extends NodeUpdate[S, BU, LU, B, L]

  case class LeafChanged[S <: Sys[S], BU, LU, B, L](leaf: L, change: LU)
    extends NodeUpdate[S, BU, LU, B, L]

  sealed trait BranchChange[S <: Sys[S], BU, LU, B, L]

  case class BranchChanged[S <: Sys[S], BU, LU, B, L](change: BU) extends BranchChange[S, BU, LU, B, L]
  
  sealed trait ChildUpdate[S <: Sys[S], BU, LU, B, L] extends BranchChange[S, BU, LU, B, L] {
    /** The position of the child among the children of the branch. */
    def idx: Int
  }
  case class ChildInserted[S <: Sys[S], BU, LU, B, L](idx: Int, node: Node[B, L])
    extends ChildUpdate[S, BU, LU, B, L]

  case class ChildRemoved[S <: Sys[S], BU, LU, B, L](idx: Int, node: Node[B, L])
    extends ChildUpdate[S, BU, LU, B, L]

  case class ChildChanged[S <: Sys[S], BU, LU, B, L](idx: Int, update: NodeUpdate[S, BU, LU, B, L])
    extends ChildUpdate[S, BU, LU, B, L]
}
trait TreeLike[S <: Sys[S], BU, LU, Repr] {
  type Leaf
  type Branch <: TreeLike.Branch[S, Branch, Leaf]

  def root: Branch

  def changed: EventLike[S, TreeLike.Update[S, BU, LU, Repr, Branch, Leaf]]
}

object SubTree extends TreeTypes {
}
class SubTree[S <: Sys[S], A, BU, LU] extends TreeLike[S, BU, LU, SubTree[S, A, BU, LU]] {
  import SubTree.Node
  import TreeLike.Update

  def changed: EventLike[S, Update[S, BU, LU, SubTree[S, A, BU, LU], Branch, Leaf]] = ???

  def root: Branch = new Branch {
    def children(implicit tx: S#Tx): data.Iterator[S#Tx, Node[Branch, Leaf]] = ???

    val name = "root"

    def size(implicit tx: S#Tx): Int = ???

    def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, Node[Branch, Leaf]] = ???

    def isEmpty(implicit tx: S#Tx): Boolean = ???

    def nonEmpty(implicit tx: S#Tx): Boolean = ???

    def apply(idx: Int)(implicit tx: S#Tx): Node[Branch, Leaf] = ???

    def indexOf(node: Node[Branch, Leaf])(implicit tx: S#Tx): Int = ???
  }

  trait Leaf {
    def name: String
  }

  trait Branch extends TreeLike.Branch[S, Branch, Leaf] {
    val name: String
  }
}

object TreeTest {
  def test1[S <: Sys[S], BU, LU, T <: TreeLike[S, BU, LU, T]](tree: T)(implicit tx: S#Tx): Unit = {
    import TreeLike.{IsLeaf, IsBranch}

    def printLeaves(b: T#Branch): Unit = b.iterator.foreach {
      case IsLeaf(l)    => println(l)
      case IsBranch(c)  => printLeaves(c)
    }

    printLeaves(tree.root)
  }

  def test2[S <: Sys[S]](tree: SubTree[S, _, _, _])(implicit tx: S#Tx): Unit = {
    tree.root.iterator.foreach {
      case Left(l) => println(l.name)
      case _ =>
    }
  }
}
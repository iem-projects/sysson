package at.iem.sysson

import de.sciss.lucre.event.EventLike
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.data

trait TreeTypes {
  // val Node           = Either
  type Node[+B, +L]     = Either[B, L]
  val IsBranch          = Left
  type IsBranch[+B, +L] = Left[B, L]
  val IsLeaf            = Right
  type IsLeaf[+B, +L]   = Right[B, L]
}

//  trait TreeCompanion[T, L, B] {
//    type Update[S <: Sys[S], A, LU] = TreeLike.Update[S, A, BU, LU, T, L, B]
//    val Update = TreeLike.Update
//  }

object TreeLike extends TreeTypes {
  trait BranchLike[S <: Sys[S], A, B, L] {
    // type N = Node[B, L]

    def size    (implicit tx: S#Tx): Int
    def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, Node[B, L]]
    def isEmpty (implicit tx: S#Tx): Boolean
    def nonEmpty(implicit tx: S#Tx): Boolean
    def apply(idx: Int)(implicit tx: S#Tx): Node[B, L]
    def indexOf(node: Node[B, L])(implicit tx: S#Tx): Int
    // def indexOfNode(node: N)(implicit tx: S#Tx): Int

    // def changed: EventLike[S, Branch.Update[S, A, LU]]
  }

  trait LeafLike[A] {
    def value: A
  }

  case class Update[S <: Sys[S], BU, LU, T, B, L](tree: T, branch: BranchUpdate[S, BU, LU, B, L])

  sealed trait NodeUpdate[S <: Sys[S], BU, LU, B, L]

  case class BranchUpdate[S <: Sys[S], BU, LU, B, L](branch: B, changes: Vec[BranchChange[S, BU, LU, B, L]])
    extends NodeUpdate[S, BU, LU, B, L]

  case class LeafChanged[S <: Sys[S], BU, LU, B, L](leaf: L, change: LU)
    extends NodeUpdate[S, BU, LU, B, L]

  sealed trait BranchChange[S <: Sys[S], BU, LU, B, L]

  case class BranchChanged[S <: Sys[S], BU, LU, B, L](branch: B, change: BU)
    extends BranchChange[S, BU, LU, B, L]
  
  sealed trait ChildUpdate[S <: Sys[S], BU, LU, B, L] extends BranchChange[S, BU, LU, B, L] {
    /** The position of the child among the children of the branch. */
    def idx: Int
  }
  case class ChildInserted[S <: Sys[S], BU, LU, B, L](idx: Int, node: Node[L, B])
    extends ChildUpdate[S, BU, LU, B, L]

  case class ChildRemoved[S <: Sys[S], BU, LU, B, L](idx: Int, node: Node[L, B])
    extends ChildUpdate[S, BU, LU, B, L]

  case class ChildChanged[S <: Sys[S], BU, LU, B, L](idx: Int, update: NodeUpdate[S, BU, LU, L, B])
    extends ChildUpdate[S, BU, LU, B, L]

  sealed trait Change[S <: Sys[S], A]
}
trait TreeLike[S <: Sys[S], A, BU, LU, Repr] {
  type Leaf   <: TreeLike.LeafLike[A]
  type Branch <: TreeLike.BranchLike[S, A, Branch, Leaf]

  def root: Branch

  def changed: EventLike[S, TreeLike.Update[S, BU, LU, Repr, Branch, Leaf]]
}

object SubTree extends TreeTypes {
  // type Update[S <: Sys[S], A, LU] = TreeLike.Update[S, A, BU, LU, SubTree[S, A, LU], L, B]
}
class SubTree[S <: Sys[S], A, BU, LU] extends TreeLike[S, A, BU, LU, SubTree[S, A, BU, LU]] {
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

    // def indexOfNode(node: N)(implicit tx: S#Tx): Int = ???
  }

  trait Leaf extends TreeLike.LeafLike[A] {
    def name: String
  }

  trait Branch extends TreeLike.BranchLike[S, A, Branch, Leaf] {
    val name: String
  }
}

object TreeTest {
  def test1[S <: Sys[S], A, BU, LU, T <: TreeLike[S, A, BU, LU, T]](tree: T)(implicit tx: S#Tx): Unit = {
    import TreeLike.{IsLeaf, IsBranch}

    def printLeaves(b: T#Branch): Unit = b.iterator.foreach {
      case IsLeaf(l)    => println(l.value)
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
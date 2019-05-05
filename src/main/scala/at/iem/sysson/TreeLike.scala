/*
 *  TreeLike.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson

import de.sciss.lucre.event.Publisher
import de.sciss.lucre.stm.Sys
import de.sciss.serial

trait TreeTypes {
  type Node[+B, +L]     = Either[B, L]
  // type Node[S <: Sys[S], T <: TreeLike[S, T]] = Either[T#Branch, T#Leaf]
  val IsBranch          = Left
  type IsBranch[+B, +L] = Left[B, L]
  // type IsBranch[S <: Sys[S], T <: TreeLike[S, T]] = Left[T#Branch, T#Leaf]
  val IsLeaf            = Right
  type IsLeaf[+B, +L]   = Right[B, L]
  // type IsLeaf[S <: Sys[S], T <: TreeLike[S, T]] = Right[T#Branch, T#Leaf]
}

object TreeLike extends TreeTypes {
  trait Branch[S <: Sys[S], T <: TreeLike[S, T]] {
    def size    (implicit tx: S#Tx): Int
    def iterator(implicit tx: S#Tx): Iterator[Node[T#Branch, T#Leaf]]
    def isEmpty (implicit tx: S#Tx): Boolean
    def nonEmpty(implicit tx: S#Tx): Boolean
    def apply(idx: Int)(implicit tx: S#Tx): Node[T#Branch, T#Leaf]
    def indexOf(node: Node[T#Branch, T#Leaf])(implicit tx: S#Tx): Int
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

  case class Update[S <: Sys[S], T <: TreeLike[S, T]](tree: T, branch: BranchUpdate[S, T])

  sealed trait NodeUpdate[S <: Sys[S], T <: TreeLike[S, T]]

  case class BranchUpdate[S <: Sys[S], T <: TreeLike[S, T]](branch: T#Branch, changes: Vec[BranchChange[S, T]])
    extends NodeUpdate[S, T]

  case class LeafChanged[S <: Sys[S], T <: TreeLike[S, T]](leaf: T#Leaf, change: T#LU)
    extends NodeUpdate[S, T]

  sealed trait BranchChange[S <: Sys[S], T <: TreeLike[S, T]]

  case class BranchChanged[S <: Sys[S], T <: TreeLike[S, T]](change: T#BU) extends BranchChange[S, T]
  
  sealed trait ChildUpdate[S <: Sys[S], T <: TreeLike[S, T]] extends BranchChange[S, T] {
    /** The position of the child among the children of the branch. */
    def idx: Int
  }
  case class ChildInserted[S <: Sys[S], T <: TreeLike[S, T]](idx: Int, node: Node[T#Branch, T#Leaf])
    extends ChildUpdate[S, T]

  case class ChildRemoved[S <: Sys[S], T <: TreeLike[S, T]](idx: Int, node: Node[T#Branch, T#Leaf])
    extends ChildUpdate[S, T]

  case class ChildChanged[S <: Sys[S], T <: TreeLike[S, T]](idx: Int, update: NodeUpdate[S, T])
    extends ChildUpdate[S, T]
}
trait TreeLike[S <: Sys[S], T <: TreeLike[S, T]] extends Publisher[S, TreeLike.Update[S, T]] {
  type BU
  type LU
  type Leaf
  type Branch <: TreeLike.Branch[S, T]

  def root: T#Branch

  def branchSerializer: serial.Serializer[S#Tx, S#Acc, T#Branch]
  def leafSerializer  : serial.Serializer[S#Tx, S#Acc, T#Leaf  ]
}

//object SubTree extends TreeTypes {
//}
//class SubTree[S <: Sys[S], A, BU, LU] extends TreeLike[S, BU, LU, SubTree[S, A, BU, LU]] {
//  import SubTree.Node
//  import TreeLike.Update
//
//  def changed: EventLike[S, Update[S, BU, LU, SubTree[S, A, BU, LU], Branch, Leaf]] = ...
//
//  def root: Branch = new Branch {
//    def children(implicit tx: S#Tx): data.Iterator[S#Tx, Node[Branch, Leaf]] = ...
//
//    val name = "root"
//
//    def size(implicit tx: S#Tx): Int = ...
//
//    def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, Node[Branch, Leaf]] = ...
//
//    def isEmpty(implicit tx: S#Tx): Boolean = ...
//
//    def nonEmpty(implicit tx: S#Tx): Boolean = ...
//
//    def apply(idx: Int)(implicit tx: S#Tx): Node[Branch, Leaf] = ...
//
//    def indexOf(node: Node[Branch, Leaf])(implicit tx: S#Tx): Int = ...
//  }
//
//  trait Leaf {
//    def name: String
//  }
//
//  trait Branch extends TreeLike.Branch[S, Branch, Leaf] {
//    val name: String
//  }
//}
//
//object TreeTest {
//  def test1[S <: Sys[S], BU, LU, T <: TreeLike[S, BU, LU, T]](tree: T)(implicit tx: S#Tx): Unit = {
//    import TreeLike.{IsLeaf, IsBranch}
//
//    def printLeaves(b: T#Branch): Unit = b.iterator.foreach {
//      case IsLeaf(l)    => println(l)
//      case IsBranch(c)  => printLeaves(c)
//    }
//
//    printLeaves(tree.root)
//  }
//
//  def test2[S <: Sys[S]](tree: SubTree[S, _, _, _])(implicit tx: S#Tx): Unit = {
//    tree.root.iterator.foreach {
//      case Left(l) => println(l.name)
//      case _ =>
//    }
//  }
//}
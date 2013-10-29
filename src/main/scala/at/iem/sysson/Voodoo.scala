package at.iem.sysson

object Voodoo {
  object TreeLike {
    trait BranchLike[Node] {
      def children: Iterator[Node]
    }

    trait LeafLike[A] {
      def value: A
    }
  }
  trait TreeLike[A, Repr <: TreeLike[A, Repr]] {
    sealed trait Node

    // case class Leaf(value: A) extends Node
    type Leaf <: TreeLike.LeafLike[A]

    type Branch <: Node with TreeLike.BranchLike[Node]

    def root: Branch
  }

  def test[A, T <: TreeLike[A, T]](tree: T): Unit = {
    tree.root.children.foreach {
      case l: tree.Leaf => println(l.value)
      case _ =>
    }
  }

  class SubTree[A] extends TreeLike[A, SubTree[A]] {
    def root: Branch = new Branch {
      def children: Iterator[Node] = ???
      val name = "foo"
    }

    trait Leaf extends TreeLike.LeafLike[A] {
      def name: String
    }

    trait Branch extends Node with TreeLike.BranchLike[Node] {
      val name: String
    }
  }

  def test2[A](tree: SubTree[A]): Unit = {
    tree.root.children.foreach {
      case l: tree.Leaf => println(l.value)
      case _ =>
    }
  }
}

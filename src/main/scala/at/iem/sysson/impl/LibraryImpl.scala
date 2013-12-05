/*
 *  LibraryImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package impl

import de.sciss.lucre.{event => evt, data, expr}
import de.sciss.lucre.event.{Pull, EventLike, InMemory, Sys}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.synth.expr.Strings
import de.sciss.serial
import de.sciss.serial.{DataOutput, DataInput}
import scala.annotation.switch

object LibraryImpl {
  import TreeLike.{IsLeaf, IsBranch, BranchChanged}
  import Library.{Leaf, Branch, Update => U, BranchUpdate => BU, Renamed, SourceChanged, BranchChange, LeafChange}

  def apply[S <: Sys[S]](implicit tx: S#Tx): Library[S] = {
    val targets = evt.Targets[S]
    val root    = newBranch[S](Strings.newConst("root"))
    new Impl(targets, root)
  }

  private type NU[S <: Sys[S]] = TreeLike.NodeUpdate[S, Library[S]]

  private def readNodeCookie(in: DataInput): Unit = {
    val ver     = in.readInt()
    require(ver == NODE_SER_VERSION, s"Unexpected node version. Found $ver, required $NODE_SER_VERSION")
  }

  private object NodeImpl {
    implicit def serializer[S <: Sys[S]]: evt.Serializer[S, NodeImpl[S]] = anySer.asInstanceOf[Ser[S]]

    def reader[S <: Sys[S]]: evt.Reader[S, NodeImpl[S]] = anySer.asInstanceOf[Ser[S]]

    // def eitherReader[S <: Sys[S]]: serial.Reader[S#Tx, S#Acc, N[S]] = anyEitherReader.asInstanceOf[EitherReader[S]]

    private def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): NodeImpl[S] = {
      val targets = evt.Targets.read(in, access)
      read(in, access, targets)
    }

    private def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                 (implicit tx: S#Tx): NodeImpl[S] = {
      readNodeCookie(in)
      (in.readByte(): @switch) match {
        case LEAF_COOKIE    => LeafImpl  .readIdentified(in, access, targets)
        case BRANCH_COOKIE  => BranchImpl.readIdentified(in, access, targets)
        case other          => sys.error(s"Unexpected cookie $other")
      }
    }

    private val anySer = new Ser[InMemory]

    // private val anyEitherReader = new EitherReader[InMemory]

    //    private final class EitherReader[S <: Sys[S]] extends serial.Reader[S#Tx, S#Acc, N[S]] {
    //      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): N[S] = NodeImpl.read(in, access).toEither
    //    }

    private final class Ser[S <: Sys[S]]
      extends serial.Serializer[S#Tx, S#Acc, NodeImpl[S]] with evt.Reader[S, NodeImpl[S]] {

      def write(n: NodeImpl[S], out: DataOutput): Unit = n.write(out)

      def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): NodeImpl[S] =
        NodeImpl.read(in, access, targets)

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): NodeImpl[S] = NodeImpl.read(in, access)
    }
  }
  sealed trait NodeImpl[S <: Sys[S]] extends evt.impl.StandaloneLike[S, NU[S], NodeImpl[S]] {
    type T = Library[S]

    def reader: evt.Reader[S, NodeImpl[S]] = NodeImpl.reader[S]
    def changed = this
    def toEither: N[S]
  }

  type N[S <: Sys[S]] = TreeLike.Node[Branch[S], Leaf[S]]

  private final val IMPL_SER_VERSION  = 0x4C696200  // "Lib\0"
  private final val NODE_SER_VERSION  = 0x4C69624E  // "LibN"
  private final val BRANCH_COOKIE     = 1
  private final val LEAF_COOKIE       = 0

  private object LeafImpl {
    def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Leaf[S]] = anySer.asInstanceOf[Ser[S]]

    private val anySer = new Ser[InMemory]

    private final class Ser[S <: Sys[S]] extends serial.Serializer[S#Tx, S#Acc, Leaf[S]] {
      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): LeafImpl[S] = LeafImpl.read(in, access)
      def write(l: Leaf[S], out: DataOutput): Unit = l.write(out)
    }

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): LeafImpl[S] = {
      val targets = evt.Targets.read(in, access)
      read(in, access, targets)
    }

    private def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                 (implicit tx: S#Tx): LeafImpl[S] = {
      readNodeCookie(in)
      val cookie  = in.readByte()
      require(cookie == LEAF_COOKIE, s"Unexpected cookie $cookie (should be $LEAF_COOKIE)")
      readIdentified(in, access, targets)
    }

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                   (implicit tx: S#Tx): LeafImpl[S] = {
      val name    = Strings.readVar(in, access)
      val source  = Strings.readVar(in, access)
      new LeafImpl(targets, name = name, source = source)
    }
  }
  private final class LeafImpl[S <: Sys[S]](val targets: evt.Targets[S],
                                            val name  : Expr.Var[S, String],
                                            val source: Expr.Var[S, String])
    extends Leaf[S] with NodeImpl[S] {

    def toEither = IsLeaf(this)

    def writeData(out: DataOutput): Unit =  {
      out.writeInt(NODE_SER_VERSION)
      out.writeByte(LEAF_COOKIE)
      name  .write(out)
      source.write(out)
    }

    def disposeData()(implicit tx: S#Tx): Unit = {
      name  .dispose()
      source.dispose()
    }

    def connect()(implicit tx: S#Tx): Unit = {
      name  .changed ---> this
      source.changed ---> this
    }

    def disconnect()(implicit tx: S#Tx): Unit = {
      name  .changed -/-> this
      source.changed -/-> this
    }

    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[NU[S]] = {
      val nameEvt = name.changed
      val nameCh  = if (pull.isOrigin(nameEvt)) pull(nameEvt).map(Renamed) else None
      val ch1     = nameCh.fold[Vec[LeafChange]](Vec.empty)(Vec(_))
      val sourceEvt = source.changed
      val sourceCh= if (pull.isOrigin(sourceEvt)) pull(sourceEvt).map(SourceChanged) else None
      val ch2     = sourceCh.fold(ch1)(ch1 :+ _)

      if (ch2.isEmpty) None else Some(TreeLike.LeafChanged[S, T](this, ch2))
    }
  }

  private object BranchImpl {
    def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Branch[S]] = anySer.asInstanceOf[Ser[S]]

    private val anySer = new Ser[InMemory]

    private final class Ser[S <: Sys[S]] extends serial.Serializer[S#Tx, S#Acc, Branch[S]] {
      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): BranchImpl[S] = BranchImpl.read(in, access)
      def write(b: Branch[S], out: DataOutput): Unit = b.write(out)
    }

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): BranchImpl[S] = {
      val targets = evt.Targets.read(in, access)
      read(in, access, targets)
    }

    private def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                 (implicit tx: S#Tx): BranchImpl[S] = {
      readNodeCookie(in)
      val cookie  = in.readByte()
      require(cookie == BRANCH_COOKIE, s"Unexpected cookie $cookie (should be $BRANCH_COOKIE)")
      readIdentified(in, access, targets)
    }

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                   (implicit tx: S#Tx): BranchImpl[S] = {
      val name    = Strings.readVar(in, access)
      val ll      = expr.LinkedList.Modifiable.read[S, NodeImpl[S], NU[S]](identity)(in, access)
      new BranchImpl(targets, name, ll)
    }
  }
  private final class BranchImpl[S <: Sys[S]](val targets: evt.Targets[S], val name: Expr[S, String],
                                              ll: expr.LinkedList.Modifiable[S, NodeImpl[S], NU[S]])
    extends Library.Branch[S] with NodeImpl[S] with evt.impl.StandaloneLike[S, BU[S], NodeImpl[S] /* BranchImpl[S] */] {

    def toEither = IsBranch(this)

    def size(implicit tx: S#Tx): Int = ll.size

    def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, N[S]] = ll.iterator.map(_.toEither)

    def isEmpty (implicit tx: S#Tx): Boolean = ll.isEmpty
    def nonEmpty(implicit tx: S#Tx): Boolean = ll.nonEmpty

    def apply(idx: Int)(implicit tx: S#Tx): N[S] = ll(idx).toEither

    def indexOf(node: N[S])(implicit tx: S#Tx): Int =
      node match {
        case IsLeaf  (l: LeafImpl  [S]) => ll.indexOf(l)
        case IsBranch(b: BranchImpl[S]) => ll.indexOf(b)
        case _                          => -1
      }

    def insertLeaf  (idx: Int, name0: Expr[S, String], source0: Expr[S, String])(implicit tx: S#Tx): Leaf[S] = {
      val targets = evt.Targets[S]
      val name    = Strings.newVar(name0  )
      val source  = Strings.newVar(source0)
      val leaf    = new LeafImpl(targets, name = name, source = source)
      ll.insert(idx, leaf)
      leaf
    }

    def insertBranch(idx: Int, name0: Expr[S, String])(implicit tx: S#Tx): Branch[S] = {
      val branch = newBranch(name0)
      ll.insert(idx, branch)
      branch
    }

    def removeAt(idx: Int)(implicit tx: S#Tx): Unit = ll.removeAt(idx)

    def remove(node: TreeLike.Node[Branch[S], Leaf[S]])(implicit tx: S#Tx): Unit = {
      val ni = node match {
        case IsLeaf  (l: LeafImpl  [S]) => l
        case IsBranch(b: BranchImpl[S]) =>
          while (!b.isEmpty) b.removeAt(b.size - 1)
          b
        case _ => sys.error(s"Not a node impl: $node")
      }
      ll.remove(ni)
      ni.dispose()
    }

    def writeData(out: DataOutput): Unit = {
      out.writeInt(NODE_SER_VERSION)
      out.writeByte(BRANCH_COOKIE)
      name.write(out)
      ll  .write(out)
    }

    def disposeData()(implicit tx: S#Tx): Unit = {
      name.dispose()
      ll  .dispose()
    }

    def connect   ()(implicit tx: S#Tx): Unit = {
      name.changed ---> this
      ll  .changed ---> this
    }
    def disconnect()(implicit tx: S#Tx) : Unit = {
      name.changed -/-> this
      ll  .changed -/-> this
    }

    def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[BU[S]] = {
      val nameEvt = name.changed
      val llEvt   = ll  .changed
      var bch     = Vec.empty[BranchChange[S]]
      if (pull.contains(nameEvt)) {
        pull(nameEvt).foreach(ch => bch :+= BranchChanged[S, T](Renamed(ch)))
      }

      if (pull.contains(llEvt)) pull(llEvt).foreach { u =>
        bch ++= u.changes.map {
          case expr.LinkedList.Added  (idx, n) => TreeLike.ChildInserted[S, T](idx, n.toEither)
          case expr.LinkedList.Removed(idx, n) => TreeLike.ChildRemoved [S, T](idx, n.toEither)
          case expr.LinkedList.Element(n, nu)  =>
            val idx = u.list.indexOf(n)
            TreeLike.ChildChanged(idx, nu)
        }
      }

      if (bch.isEmpty) None else Some(TreeLike.BranchUpdate[S, T](this, bch))
    }
  }

  private def newBranch[S <: Sys[S]](name0: Expr[S, String])(implicit tx: S#Tx): BranchImpl[S] = {
    val targets = evt.Targets[S]
    val name    = Strings.newVar(name0)
    val ll      = expr.LinkedList.Modifiable[S, NodeImpl[S], NU[S]](identity)
    new BranchImpl(targets, name, ll)
  }

  def serializer      [S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Library       [S]] = anySer.asInstanceOf[Ser[S]]
  def branchSerializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Library.Branch[S]] = BranchImpl.serializer[S]
  def leafSerializer  [S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Library.Leaf  [S]] = LeafImpl  .serializer[S]

  private def reader[S <: Sys[S]]: evt.Reader[S, Impl[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[InMemory]

  private final class Ser[S <: Sys[S]] extends evt.Reader[S, Impl[S]] with serial.Serializer[S#Tx, S#Acc, Library[S]] {
    def write(lib: Library[S], out: DataOutput): Unit = lib.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Library[S] = {
      val targets = evt.Targets.read(in, access)
      read(in, access, targets)
    }

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Impl[S] = {
      val ver     = in.readInt()
      require(ver == IMPL_SER_VERSION, s"Unexpected library version. Found $ver, required $IMPL_SER_VERSION")
      val root    = BranchImpl.read(in, access)
      new Impl(targets, root)
    }
  }

  private final class Impl[S <: Sys[S]](val targets: evt.Targets[S], _root: BranchImpl[S])
    extends Library[S] with evt.impl.StandaloneLike[S, U[S], Impl[S]] {

    private type T = Library[S]

    def writeData(out: DataOutput): Unit = {
      out.writeInt(IMPL_SER_VERSION)
      _root.write(out)
    }

    def disposeData()(implicit tx: S#Tx): Unit = {
      _root.dispose()
    }

    def connect   ()(implicit tx: S#Tx): Unit = _root ---> this
    def disconnect()(implicit tx: S#Tx): Unit = _root -/-> this

    def reader: evt.Reader[S, Impl[S]] = LibraryImpl.reader[S]

    def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[U[S]] = pull(_root).map(TreeLike.Update[S, T](this, _))

    def root: Branch = _root

    def changed: EventLike[S, U[S]] = this

    // def nodeReader: serial.Reader[S#Tx, S#Acc, N[S]] = NodeImpl.eitherReader[S]

    def branchSerializer: serial.Serializer[S#Tx, S#Acc, Library.Branch[S]] = BranchImpl.serializer
    def leafSerializer  : serial.Serializer[S#Tx, S#Acc, Library.Leaf  [S]] = LeafImpl  .serializer
  }
}
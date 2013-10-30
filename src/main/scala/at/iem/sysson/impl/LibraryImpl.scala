//package at.iem.sysson
//package impl
//
//import de.sciss.lucre.synth.{InMemory, Sys}
//import de.sciss.lucre.{event => evt}
//import de.sciss.lucre.event.{Targets, Pull, EventLike}
//import de.sciss.lucre.expr.Expr
//import de.sciss.lucre.{data, expr}
//import de.sciss.lucre.synth.expr.Strings
//import de.sciss.serial
//import de.sciss.serial.{DataOutput, DataInput}
//import de.sciss.lucre.stm.Mutable
//
//object LibraryImpl {
//  import TreeLike.{Node, IsLeaf, IsBranch, BranchChanged}
//  import Library.{Leaf, Branch, Update => U, BranchUpdate => BU, Renamed, LeafUpdate => LU, BranchChange}
//
//  def apply[S <: Sys[S]](implicit tx: S#Tx): Library[S] = {
//    val targets = evt.Targets[S]
//    val root    = newBranch[S](Strings.newConst("root"))
//    new Impl(targets, root)
//  }
//
//  // private type N [S <: Sys[S]] = Node[BranchImpl[S], LeafImpl[S]]
//  private type N [S <: Sys[S]] = Node[Branch[S], Leaf[S]]
//  private type NU[S <: Sys[S]] = TreeLike.NodeUpdate[S, Renamed, LU, Branch[S], Leaf[S]]
//
//  private implicit def nodeSerializer[S <: Sys[S]]: evt.Serializer[S, N[S]] = anyNodeSer.asInstanceOf[NodeSer[S]]
//
//  private val anyNodeSer = new NodeSer[InMemory]
//
//  private final class NodeSer[S <: Sys[S]] extends serial.Serializer[S#Tx, S#Acc, N[S]] with evt.Reader[S, N[S]] {
//    def write(n: N[S], out: DataOutput): Unit = n match {
//      case IsLeaf  (l) => l.write(out)
//      case IsBranch(b) => b.write(out)
//    }
//
//    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): N[S] with Node[S]
//
//    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx) = ??? : LibraryImpl.N[S]
//  }
//
//  private object LeafImpl {
//    final val SER_VERSION = 0x4C69624C  // "LibL"
//
//    implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Leaf /* Impl */[S]] =
//      anySer.asInstanceOf[evt.Serializer[S, Leaf /* Impl */[S]]]
//
//    def reader[S <: Sys[S]]: evt.Reader[S, LeafImpl[S]] = anySer.asInstanceOf[Ser[S]]
//
//    private val anySer = new Ser[InMemory]
//
//    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): LeafImpl[S] = {
//      val targets = evt.Targets.read(in, access)
//      read(in, access, targets)
//    }
//
//    def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): LeafImpl[S] = {
//      val cookie  = in.readInt()
//      require(cookie == SER_VERSION, s"Unexpected cookie $cookie (should be $SER_VERSION)")
//      val name    = Strings.readVar(in, access)
//      val source  = Strings.readVar(in, access)
//      new LeafImpl(targets, name = name, source = source)
//    }
//
//    private final class Ser[S <: Sys[S]]
//      extends serial.Serializer[S#Tx, S#Acc, Leaf /* Impl */[S]] with evt.Reader[S, LeafImpl[S]] {
//
//      def write(l: Leaf /* Impl */[S], out: DataOutput): Unit = l.write(out)
//
//      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): LeafImpl[S] = LeafImpl.read(in, access)
//
//      def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): LeafImpl[S] =
//        LeafImpl.read(in, access, targets)
//    }
//  }
//  private final class LeafImpl[S <: Sys[S]](val targets: evt.Targets[S],
//                                            val name  : Expr.Var[S, String],
//                                            val source: Expr.Var[S, String])
//    extends Leaf[S] /* with Mutable.Impl[S] */ with evt.impl.StandaloneLike[S, LU, LeafImpl[S]] {
//
//    def writeData(out: DataOutput): Unit =  {
//      out.writeInt(LeafImpl.SER_VERSION)
//      name  .write(out)
//      source.write(out)
//    }
//
//    def disposeData()(implicit tx: S#Tx): Unit = {
//      name  .dispose()
//      source.dispose()
//    }
//
//    def disconnect()(implicit tx: S#Tx): Unit = {
//      name  .changed ---> this
//      source.changed ---> this
//    }
//
//    def connect()(implicit tx: S#Tx): Unit = {
//      name  .changed -/-> this
//      source.changed -/-> this
//    }
//
//    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[LU] = {
//      ???
//    }
//
//    def reader: evt.Reader[S, LeafImpl[S]] = LeafImpl.reader[S]
//  }
//
//  private object BranchImpl {
//    final val SER_VERSION = 0x4C696242  // "LibB"
//
//    implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Branch /* Impl */[S]] =
//      anySer.asInstanceOf[evt.Serializer[S, Branch /* Impl */[S]]]
//
//    def reader[S <: Sys[S]]: evt.Reader[S, BranchImpl[S]] = anySer.asInstanceOf[Ser[S]]
//
//    private val anySer = new Ser[InMemory]
//
//    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): BranchImpl[S] = {
//      val targets = evt.Targets.read(in, access)
//      read(in, access, targets)
//    }
//
//    def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): BranchImpl[S] = {
//      val cookie  = in.readInt()
//      require(cookie == SER_VERSION, s"Unexpected cookie $cookie (should be $SER_VERSION)")
//      val name    = Strings.readVar(in, access)
//      val ll      = expr.LinkedList.Modifiable.read[S, N[S], NU[S]] {
//        case IsLeaf  (l) => l.changed
//        case IsBranch(b) => b.changed
//      } (in, access)
//      new BranchImpl(targets, name, ll)
//    }
//
//    private final class Ser[S <: Sys[S]]
//      extends serial.Serializer[S#Tx, S#Acc, Branch /* Impl */[S]] with evt.Reader[S, BranchImpl[S]] {
//
//      def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): BranchImpl[S] =
//        BranchImpl.read(in, access, targets)
//
//      def write(b: Branch /* Impl */[S], out: DataOutput): Unit = b.write(out)
//
//      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): BranchImpl[S] = BranchImpl.read(in, access)
//    }
//  }
//  private final class BranchImpl[S <: Sys[S]](val targets: evt.Targets[S], val name: Expr[S, String],
//                                              ll: expr.LinkedList.Modifiable[S, N[S], NU[S]])
//    extends Library.Branch[S] with /* Mutable.Impl[S] with */ evt.impl.StandaloneLike[S, BU[S], BranchImpl[S]] {
//
//    def size(implicit tx: S#Tx): Int = ll.size
//
//    def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, Node[Branch[S], Leaf[S]]] = ll.iterator
//
//    def isEmpty (implicit tx: S#Tx): Boolean = ll.isEmpty
//    def nonEmpty(implicit tx: S#Tx): Boolean = ll.nonEmpty
//
//    def apply(idx: Int)(implicit tx: S#Tx): Node[Branch[S], Leaf[S]] = ll(idx)
//
//    def indexOf(node: Node[Branch[S], Leaf[S]])(implicit tx: S#Tx): Int = node match {
//      case IsLeaf  (l: LeafImpl  [S]) => ll.indexOf(IsLeaf  (l))
//      case IsBranch(b: BranchImpl[S]) => ll.indexOf(IsBranch(b))
//      case _                          => -1
//    }
//
//    def insertLeaf  (idx: Int, name0: Expr[S, String], source0: Expr[S, String])(implicit tx: S#Tx): Leaf[S] = {
//      val targets = evt.Targets[S]
//      val name    = Strings.newVar(name0  )
//      val source  = Strings.newVar(source0)
//      val leaf    = new LeafImpl(targets, name = name, source = source)
//      ll.insert(idx, IsLeaf(leaf))
//      leaf
//    }
//
//    def insertBranch(idx: Int, name0: Expr[S, String])(implicit tx: S#Tx): Branch[S] = {
//      val branch = newBranch(name0)
//      ll.insert(idx, IsBranch(branch))
//      branch
//    }
//
//    def writeData(out: DataOutput): Unit = {
//      out.writeInt(BranchImpl.SER_VERSION)
//      name.write(out)
//      ll  .write(out)
//    }
//
//    def disposeData()(implicit tx: S#Tx): Unit = {
//      name.dispose()
//      ll  .dispose()
//    }
//
//    def connect   ()(implicit tx: S#Tx): Unit = {
//      name.changed ---> this
//      ll  .changed ---> this
//    }
//    def disconnect()(implicit tx: S#Tx) : Unit = {
//      name.changed -/-> this
//      ll  .changed -/-> this
//    }
//
//    def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[BU[S]] = {
//      val nameEvt = name.changed
//      val llEvt   = ll  .changed
//      var bch     = Vec.empty[BranchChange[S]]
//      if (pull.isOrigin(nameEvt)) {
//        pull(nameEvt).foreach(ch => bch :+= BranchChanged(Renamed(ch)))
//      }
//
//      if (pull.isOrigin(llEvt)) pull(llEvt).fold(Vec.empty[BranchChange[S]]) { u =>
//        u.changes.map {
//          case expr.LinkedList.Added  (idx, n) => TreeLike.ChildInserted(idx, n)
//          case expr.LinkedList.Removed(idx, n) => TreeLike.ChildRemoved (idx, n)
//          case expr.LinkedList.Element(n, nu)  =>
//            val idx = u.list.indexOf(n)
//            TreeLike.ChildChanged(idx, nu)
//        }
//        ???
//      }
//
//      if (bch.isEmpty) None else Some(TreeLike.BranchUpdate(this, bch))
//    }
//
//    def reader: evt.Reader[S, BranchImpl[S]] = BranchImpl.reader[S]
//  }
//
//  private def newBranch[S <: Sys[S]](name0: Expr[S, String])(implicit tx: S#Tx): BranchImpl[S] = {
//    // val id    = tx.newID()
//    val targets = evt.Targets[S]
//    val name    = Strings.newVar(name0)
//    val ll      = expr.LinkedList.Modifiable[S, N[S], NU[S]] {
//      case IsLeaf  (l) => l.changed
//      case IsBranch(b) => b.changed
//    }
//    new BranchImpl(targets, name, ll)
//  }
//
//  private final val IMPL_SER_VERSION = 0x4C696200  // "Lib\0"
//
//  def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Library[S]] =
//    anySer.asInstanceOf[Ser[S]]
//
//  private def reader[S <: Sys[S]]: evt.Reader[S, Impl[S]] = anySer.asInstanceOf[Ser[S]]
//
//  private val anySer = new Ser[InMemory]
//
//  private final class Ser[S <: Sys[S]] extends evt.Reader[S, Impl[S]] with serial.Serializer[S#Tx, S#Acc, Library[S]] {
//    def write(lib: Library[S], out: DataOutput): Unit = lib.write(out)
//
//    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Library[S] = {
//      val targets = evt.Targets.read(in, access)
//      read(in, access, targets)
//    }
//
//    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Impl[S] = {
//      val root    = BranchImpl.read(in, access)
//      new Impl(targets, root)
//    }
//  }
//
//  private final class Impl[S <: Sys[S]](val targets: evt.Targets[S], _root: BranchImpl[S])
//    extends Library[S] with evt.impl.StandaloneLike[S, U[S], Impl[S]] {
//
//    def writeData(out: DataOutput): Unit = {
//      out.writeInt(IMPL_SER_VERSION)
//      _root.write(out)
//    }
//
//    def disposeData()(implicit tx: S#Tx): Unit = {
//      _root.dispose()
//    }
//
//    def connect   ()(implicit tx: S#Tx): Unit = _root ---> this
//    def disconnect()(implicit tx: S#Tx): Unit = _root -/-> this
//
//    def reader: evt.Reader[S, Impl[S]] = LibraryImpl.reader[S]
//
//    def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[U[S]] = pull(_root).map(TreeLike.Update(this, _))
//
//    def root: Branch = _root
//
//    def changed: EventLike[S, U[S]] = this
//  }
//}
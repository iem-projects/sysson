package at.iem.sysson
package gui
package impl

import swing.{ScrollPane, Component}
import de.sciss.lucre.stm.{Disposable, IdentifierMap}
import de.sciss.model.impl.ModelImpl
import javax.swing.DropMode
import de.sciss.treetable.{TreeTableSelectionChanged, TreeTableCellRenderer, TreeColumnModel, AbstractTreeModel, TreeTable}
import GUI.{fromTx => guiFromTx, requireEDT}
import de.sciss.lucre.event.Sys
import TreeLike.{IsBranch, IsLeaf}

object TreeTableViewImpl {
  private final val DEBUG = false

  private object NodeView {
    def apply[S <: Sys[S], T <: TreeLike[S, T], D](parent: BranchOrRoot[S, T, D], data: D,
                                                   c: TreeLike.Node[T#Branch, T#Leaf]): NodeView[S, T, D] = c match {
        case IsBranch(cb) =>
          new NodeView.Branch(parent, data)
        case IsLeaf  (cl) =>
          new NodeView.Leaf(parent, data)
      }

    sealed trait OrRoot[S <: Sys[S], T <: TreeLike[S, T], D]

    sealed trait BranchOrRoot[S <: Sys[S], T <: TreeLike[S, T], D] extends OrRoot[S, T, D] {
      /** The children of the folder. This variable _must only be accessed or updated_ on the event thread. */
      var children = Vec.empty[NodeView[S, T, D]]
    }

    class Branch[S <: Sys[S], T <: TreeLike[S, T], D](val parent: BranchOrRoot[S, T, D], var data: D)
      extends BranchOrRoot[S, T, D] with NodeView[S, T, D]

    class Root[S <: Sys[S], T <: TreeLike[S, T], D] extends BranchOrRoot[S, T, D]
    
    class Leaf[S <: Sys[S], T <: TreeLike[S, T], D](val parent: BranchOrRoot[S, T, D], var data: D)
      extends NodeView[S, T, D]
  }
  private sealed trait NodeView[S <: Sys[S], T <: TreeLike[S, T], D] extends NodeView.OrRoot[S, T, D] {
    var data  : D
    def parent: NodeView.BranchOrRoot[S, T, D]
  }

  def apply[S <: Sys[S], T <: TreeLike[S, T], D](tree: T, handler: TreeTableView.Handler[S, T, D])
                                             (implicit tx: S#Tx): TreeTableView       [S, T, D] = {
    type Branch = T#Branch
    val _handler = handler
    val root: T#Branch = tree.root
    new Impl[S, T, D] {
      val mapViews  = tx.newInMemoryIDMap[VNode]  // folder IDs to renderers
      val rootView  = new NodeView.Root[S, T, D]
      val handler    = _handler

      private def buildMapView(f: Branch, fv: NodeView.BranchOrRoot[S, T, D]): Unit = {
        val tup = f.iterator.map { c =>
          val data  = handler.viewData(c)
          val v     = NodeView(fv, data, c)
          c -> v
        } .toIndexedSeq
        fv.children = tup.map(_._2)
        tup.foreach { case (c, cv) =>
          val id = handler.nodeID(c)
          mapViews.put(id, cv)
          (c, cv) match {
            case (IsBranch(cf), cfv: NodeView.Branch[S, T, D]) =>
              buildMapView(cf, cfv)
            case _ =>
          }
        }
      }
      buildMapView(root, rootView)

      val observer: Disposable[S#Tx] = tree.changed.react { implicit tx => upd =>
        folderUpdated(rootView, upd.branch.changes)
      }

      guiFromTx {
        guiInit()
      }
    }
  }

  private abstract class Impl[S <: Sys[S], T <: TreeLike[S, T], D]
    extends ComponentHolder[Component] with TreeTableView[S, T, D] with ModelImpl[Any /* BranchView.Update[S] */] {
    view =>

    type V        = NodeView.OrRoot       [S, T, D]
    type VBranch  = NodeView.BranchOrRoot [S, T, D]
    type VBranchI = NodeView.Branch       [S, T, D]
    type VNode    = NodeView              [S, T, D]
    type TPath    = TreeTable.Path[VBranch]
    type TNode    = TreeLike.Node[T#Branch, T#Leaf]

    protected def rootView: NodeView.Root [S, T, D]
    protected def mapViews: IdentifierMap[S#ID, S#Tx, VNode]
    protected def observer: Disposable[S#Tx]
    protected def handler  : TreeTableView.Handler[S, T, D]

    private class ElementTreeModel extends AbstractTreeModel[V] {
      lazy val root: V = rootView // ! must be lazy. suckers....

      def getChildCount(parent: V): Int = parent match {
        case b: VBranch => b.children.size
        case _          => 0
      }

      def getChild(parent: V, index: Int): VNode = parent match {
        case b: VBranch => b.children(index)
        case _ => sys.error(s"parent $parent is not a branch")
      }

      def isLeaf(node: V): Boolean = node match {
        case _: VBranch => false
        case _          => true
      }

      def getIndexOfChild(parent: V, child: V): Int = parent match {
        case b: VBranch => b.children.indexOf(child)
        case _          => sys.error(s"parent $parent is not a branch")
      }

      def getParent(node: V): Option[V] = node match {
        case n: VNode => Some(n.parent)
        case _        => None
      }

      def valueForPathChanged(path: TreeTable.Path[V], newValue: V): Unit =
        println(s"valueForPathChanged($path, $newValue)")

      def elemAdded(parent: VBranch, idx: Int, view: VNode): Unit = {
        if (DEBUG) println(s"model.elemAdded($parent, $idx, $view)")
        val g       = parent  // Option.getOrElse(_root)
        require(idx >= 0 && idx <= g.children.size)
        g.children  = g.children.patch(idx, Vector(view), 0)
        fireNodesInserted(view)
      }

      def elemRemoved(parent: VBranch, idx: Int): Unit = {
        if (DEBUG) println(s"model.elemRemoved($parent, $idx)")
        require(idx >= 0 && idx < parent.children.size)
        val v       = parent.children(idx)
        // this is frickin insane. the tree UI still accesses the model based on the previous assumption
        // about the number of children, it seems. therefore, we must not update children before
        // returning from fireNodesRemoved.
        fireNodesRemoved(v)
        parent.children  = parent.children.patch(idx, Vector.empty, 1)
      }

      def elemUpdated(view: VNode): Unit = {
        if (DEBUG) println(s"model.elemUpdated($view)")
        fireNodesChanged(view)
      }
    }

    private var _model: ElementTreeModel  = _
    private var t: TreeTable[V, TreeColumnModel[V]] = _

    def treeTable: TreeTable[_, _] = t

    def elemAdded(parent: VBranch, idx: Int, elem: TNode)(implicit tx: S#Tx): Unit = {
      if (DEBUG) println(s"elemAdded($parent, $idx $elem)")
      val data  = handler.viewData(elem)
      val v     = NodeView[S, T, D](parent, data, elem)
      val id    = handler.nodeID(elem)
      mapViews.put(id, v)

      guiFromTx {
        _model.elemAdded(parent, idx, v)
      }

      (elem, v) match {
        case (IsBranch(f), fv: VBranchI) if !f.isEmpty =>
          f.iterator.toList.zipWithIndex.foreach { case (c, ci) =>
            elemAdded(fv, ci, c)
          }

        case _ =>
      }
    }

    def elemRemoved(parent: VBranch, idx: Int, elem: TNode)(implicit tx: S#Tx): Unit = {
      if (DEBUG) println(s"elemRemoved($parent, $idx, $elem)")
      val id  = handler.nodeID(elem)
      mapViews.get(id).foreach { v =>
        (elem, v) match {
          case (IsBranch(f), fv: VBranchI) if f.nonEmpty =>
            f.iterator.toList.zipWithIndex.reverse.foreach { case (c, ci) =>
              elemRemoved(fv, ci, c)
            }

          case _ =>
        }

        mapViews.remove(id)

        guiFromTx {
          _model.elemRemoved(parent, idx)
        }
      }
    }

    def branchElemUpdated(b: T#Branch, upd: T#BU)(implicit tx: S#Tx): Unit = {
      val id      = handler.nodeID(IsBranch(b))
      val viewOpt = mapViews.get(id)
      if (viewOpt.isEmpty) {
        println(s"WARNING: No view for branch $b")
      }
      viewOpt.foreach { v =>
        val oldData = v.data
        val newData = handler.branchUpdate(b, upd, oldData)
        if (newData != oldData) guiFromTx {
          v.data = newData
          _model.elemUpdated(v)
        }
      }
    }

    def folderUpdated(parent: VBranch, changes: Vec[TreeLike.BranchChange[S, T]])
                     (implicit tx: S#Tx): Unit = changes.foreach {
      case TreeLike.BranchChanged(bch)        => ??? // elemUpdated()
      case TreeLike.ChildInserted(idx, child) => elemAdded  (parent, idx, child)
      case TreeLike.ChildRemoved (idx, child) => elemRemoved(parent, idx, child)
      case TreeLike.ChildChanged (idx, cch)   => cch match {
        case TreeLike.BranchUpdate(parent1, bch1) =>
          val id = handler.nodeID(IsBranch(parent1))
          mapViews.get(id) match {
            case Some(vparent1: VBranch) => folderUpdated(vparent1, bch1)
            case _ => println(s"Warning: No view found for $parent1")
          }

        case TreeLike.LeafChanged(l, lch1) =>
          ???
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      observer.dispose()
      mapViews.dispose()
    }

    protected def guiInit(): Unit = {
      requireEDT()

      _model = new ElementTreeModel

      val tcm = new TreeColumnModel[V] {
        private val peer = handler.columns

        def getValueAt(r: V, column: Int): Any = r match {
          case node: VNode => peer.getValueAt(node.data, column)
          case _ => () // XXX
        }

        def setValueAt(value: Any, r: V, column: Int): Unit = r match {
          case node: VNode => peer.setValueAt(value, node.data, column)
          case _ => throw new IllegalStateException(s"Trying to alter $r")
        }

        def getColumnName (column: Int): String   = peer.getColumnName (column)
        def getColumnClass(column: Int): Class[_] = peer.getColumnClass(column)

        def columnCount: Int = peer.columnCount

        def isCellEditable(r: V, column: Int): Boolean = r match {
          case node: VNode => peer.isCellEditable(node.data, column)
          case _ => false
        }

        def hierarchicalColumn: Int = peer.hierarchicalColumn
      }

      t = new TreeTable(_model, tcm: TreeColumnModel[V])
      t.rootVisible = false
      t.renderer    = new TreeTableCellRenderer {
        def getRendererComponent(treeTable: TreeTable[_, _], value: Any, row: Int, column: Int,
                                 state: TreeTableCellRenderer.State): Component = {
          val node = value.asInstanceOf[VNode]
          handler.renderer(view, node.data, row = row, column = column, state = state)
        }
      }
      val tabCM = t.peer.getColumnModel
      tabCM.getColumn(0).setPreferredWidth(176)
      tabCM.getColumn(1).setPreferredWidth(256)

      t.listenTo(t.selection)
      t.reactions += {
        case e: TreeTableSelectionChanged[_, _] =>  // this crappy untyped event doesn't help us at all
          // println(s"selection: $e")
          // dispatch(BranchView.SelectionChanged(view, selection))
        // case e => println(s"other: $e")
      }
      t.showsRootHandles  = true
      t.expandPath(TreeTable.Path(_model.root))
      t.dragEnabled       = true
      t.dropMode          = DropMode.ON_OR_INSERT_ROWS

      val scroll    = new ScrollPane(t)
      scroll.border = null
      component     = scroll
    }

    //    def selection: BranchView.Selection[S] =
    //      t.selection.paths.collect({
    //        case PathExtrator(path, child) => (path, child)
    //      })(breakOut)
    //
    //    object PathExtrator {
    //      def unapply(path: Seq[Node]): Option[(Vec[NodeView.BranchLike[S]], NodeView[S])] =
    //        path match {
    //          case init :+ (last: NodeView[S]) =>
    //            val pre: Vec[NodeView.BranchLike[S]] = init.map({
    //              case g: NodeView.BranchLike[S] => g
    //              case _ => return None
    //            })(breakOut)
    //            Some((/* _root +: */ pre, last))
    //          case _ => None
    //        }
    //    }
  }
}
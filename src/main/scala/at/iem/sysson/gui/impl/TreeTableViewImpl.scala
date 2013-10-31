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
import at.iem.sysson.gui.TreeTableView.Handler

object TreeTableViewImpl {
  private final val DEBUG = false

  private object NodeView {
    sealed trait OrRoot[S <: Sys[S], T <: TreeLike[S, T], H <: Handler[S, T, H]]

    sealed trait BranchOrRoot[S <: Sys[S], T <: TreeLike[S, T], H <: Handler[S, T, H]] extends OrRoot[S, T, H] {
      /** The children of the folder. This variable _must only be accessed or updated_ on the event thread. */
      var children = Vec.empty[NodeView[S, T, H]]
      var data: H#BD
    }

    class Branch[S <: Sys[S], T <: TreeLike[S, T], H <: Handler[S, T, H]](val parent: BranchOrRoot[S, T, H],
                                                                          var data: H#BD)
      extends BranchOrRoot[S, T, H] with NodeView[S, T, H]

    class Root[S <: Sys[S], T <: TreeLike[S, T], H <: Handler[S, T, H]](var data: H#BD) extends BranchOrRoot[S, T, H]
    
    class Leaf[S <: Sys[S], T <: TreeLike[S, T], H <: Handler[S, T, H]](val parent: BranchOrRoot[S, T, H],
                                                                        var data: H#LD)
      extends NodeView[S, T, H]
  }
  private sealed trait NodeView[S <: Sys[S], T <: TreeLike[S, T], H <: Handler[S, T, H]]
    extends NodeView.OrRoot[S, T, H] {

    def data: H#D
    def parent: NodeView.BranchOrRoot[S, T, H]
  }

  def apply[S <: Sys[S], T <: TreeLike[S, T], H <: Handler[S, T, H]](tree: T, handler: H)
                                                                    (implicit tx: S#Tx): TreeTableView[S, T, H] = {
    type Branch = T#Branch
    val _handler = handler
    val root: T#Branch = tree.root
    new Impl[S, T, H] {
      val mapViews  = tx.newInMemoryIDMap[VNode]  // folder IDs to renderers
      val rootView  = new NodeView.Root[S, T, H](_handler.branchData(root))
      val handler    = _handler

      private def buildMapView(f: Branch, fv: NodeView.BranchOrRoot[S, T, H]): Unit = {
        val vs = f.iterator.map { c =>
          val v = c match {
            case IsLeaf(l) =>
              val data  = handler.leafData(l)
              val id    = handler.leafID(l)
              val _v    = new NodeView.Leaf[S, T, H](fv, data)
              mapViews.put(id, _v)
              _v

            case IsBranch(b) =>
              val data  = handler.branchData(b)
              val id    = handler.branchID(b)
              val _v    = new NodeView.Branch[S, T, H](fv, data)
              mapViews.put(id, _v)
              buildMapView(b, _v)
              _v
          }
          v
        } .toIndexedSeq
        fv.children = vs
      }
      buildMapView(root, rootView)

      val observer: Disposable[S#Tx] = tree.changed.react { implicit tx => upd =>
        folderUpdated(rootView, upd.branch)
      }

      guiFromTx {
        guiInit()
      }
    }
  }

  private abstract class Impl[S <: Sys[S], T <: TreeLike[S, T], H <: Handler[S, T, H]]
    extends ComponentHolder[Component] with TreeTableView[S, T, H] with ModelImpl[Any /* BranchView.Update[S] */] {
    view =>

    type V        = NodeView.OrRoot       [S, T, H]
    type VBranch  = NodeView.BranchOrRoot [S, T, H]
    type VBranchI = NodeView.Branch       [S, T, H]
    type VLeaf    = NodeView.Leaf         [S, T, H]
    type VNode    = NodeView              [S, T, H]
    type TPath    = TreeTable.Path[VBranch]
    type TNode    = TreeLike.Node[T#Branch, T#Leaf]

    protected def rootView: NodeView.Root [S, T, H]
    protected def mapViews: IdentifierMap[S#ID, S#Tx, VNode]
    protected def observer: Disposable[S#Tx]
    protected def handler : H

    private class ElementTreeModel extends AbstractTreeModel[V] {
      lazy val root: V = rootView // ! must be lazy. suckers....

      def getChildCount(parent: V): Int = parent match {
        case b: VBranch => b.children.size
        case _          => 0
      }

      def getChild(parent: V, index: Int): VNode = parent match {
        case b: VBranch => b.children(index)
        case _          => sys.error(s"parent $parent is not a branch")
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

      def addView(id: S#ID, v: VNode): Unit = {
        mapViews.put(id, v)

        guiFromTx {
          _model.elemAdded(parent, idx, v)
        }
      }

      elem match {
        case IsLeaf(l) =>
          val data  = handler.leafData(l)
          val id    = handler.leafID(l)
          val v     = new NodeView.Leaf(parent, data)
          addView(id, v)

        case IsBranch(b) =>
          val data  = handler.branchData(b)
          val id    = handler.branchID(b)
          val v     = new NodeView.Branch(parent, data)
          addView(id, v)
          if (b.nonEmpty) b.iterator.toList.zipWithIndex.foreach { case (c, ci) =>
            elemAdded(v, ci, c)
          }
      }
    }

    def elemRemoved(parent: VBranch, idx: Int, elem: TNode)(implicit tx: S#Tx): Unit = {
      if (DEBUG) println(s"elemRemoved($parent, $idx, $elem)")

      def removeView(id: S#ID): Unit = {
        mapViews.remove(id)
        guiFromTx {
          _model.elemRemoved(parent, idx)
        }
      }

      elem match {
        case IsLeaf(l) =>
          val id  = handler.leafID(l)
          removeView(id)

        case IsBranch(b) =>
          val id  = handler.branchID(b)
          if (b.nonEmpty) mapViews.get(id) match {
            case Some(fv: VBranchI) =>
              b.iterator.toList.zipWithIndex.reverse.foreach { case (c, ci) =>
                elemRemoved(fv, ci, c)
              }

            case other => warnNoBranchView(b, other)
          }

          removeView(id)
      }
    }

    def branchElemUpdated(b: T#Branch, upd: T#BU)(implicit tx: S#Tx): Unit = {
      val id = handler.branchID(b)
      mapViews.get(id) match {
        case Some(v: VBranch) =>
          val oldData = v.data
          val newData = handler.branchUpdate(b, upd, oldData)
          if (newData != oldData) guiFromTx {
            v.data = newData
            _model.elemUpdated(v)
          }
        case other => warnNoBranchView(b, other)
      }
    }

    def leafElemUpdated(l: T#Leaf, upd: T#LU)(implicit tx: S#Tx): Unit = {
      val id = handler.leafID(l)
      mapViews.get(id) match {
        case Some(v: VLeaf) =>
          val oldData = v.data
          val newData = handler.leafUpdate(l, upd, oldData)
          if (newData != oldData) guiFromTx {
            v.data = newData
            _model.elemUpdated(v)
          }
        case other => warnNoLeafView(l, other)
      }
    }

    private def warnNoBranchView(b: T#Branch, found: Any): Unit =
      println(s"Warning: should find a branch view for $b but got $found")

    private def warnNoLeafView(b: T#Leaf, found: Any): Unit =
      println(s"Warning: should find a leaf view for $b but got $found")

    def folderUpdated(parent: VBranch, upd: TreeLike.BranchUpdate[S, T])
                     (implicit tx: S#Tx): Unit = upd.changes.foreach {
      case TreeLike.BranchChanged(bch)        => branchElemUpdated(upd.branch, bch)
      case TreeLike.ChildInserted(idx, child) => elemAdded  (parent, idx, child)
      case TreeLike.ChildRemoved (idx, child) => elemRemoved(parent, idx, child)
      case TreeLike.ChildChanged (idx, cch)   => cch match {
        case upd1 @ TreeLike.BranchUpdate(parent1, _) =>
          val id = handler.branchID(parent1)
          mapViews.get(id) match {
            case Some(vparent1: VBranch)  => folderUpdated(vparent1, upd1)
            case other                    => warnNoBranchView(parent1, other)
          }

        case TreeLike.LeafChanged(l, lch1) => leafElemUpdated(l, lch1)
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
          case node: VNode  => peer.getValueAt(node.data, column)
          case _            => null // XXX correct?
        }

        def setValueAt(value: Any, r: V, column: Int): Unit = r match {
          case node: VNode  => peer.setValueAt(value, node.data, column)
          case _            => throw new IllegalStateException(s"Trying to alter $r")
        }

        def getColumnName (column: Int): String   = peer.getColumnName (column)
        def getColumnClass(column: Int): Class[_] = peer.getColumnClass(column)

        def columnCount: Int = peer.columnCount

        def isCellEditable(r: V, column: Int): Boolean = r match {
          case node: VNode  => peer.isCellEditable(node.data, column)
          case _            => false
        }

        def hierarchicalColumn: Int = peer.hierarchicalColumn
      }

      t = new TreeTable(_model, tcm: TreeColumnModel[V])
      t.rootVisible = false
      t.renderer    = new TreeTableCellRenderer {
        def getRendererComponent(treeTable: TreeTable[_, _], value: Any, row: Int, column: Int,
                                 state: TreeTableCellRenderer.State): Component = {
          value match {
            case b: VBranchI  => handler.branchRenderer(view, b.data, row = row, column = column, state = state)
            case l: VLeaf     => handler.leafRenderer  (view, l.data, row = row, column = column, state = state)
            case _ => null
          }
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
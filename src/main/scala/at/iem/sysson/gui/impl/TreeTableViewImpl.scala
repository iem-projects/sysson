package at.iem.sysson
package gui
package impl

import scala.swing.{ScrollPane, Component}
import de.sciss.lucre.stm.{Disposable, IdentifierMap}
import de.sciss.model.impl.ModelImpl
import javax.swing.DropMode
import de.sciss.treetable.{j, TreeTableSelectionChanged, TreeTableCellRenderer, TreeColumnModel, AbstractTreeModel, TreeTable}
import de.sciss.lucre.event.Sys
import TreeLike.{IsBranch, IsLeaf}
import at.iem.sysson.gui.TreeTableView.Handler
import javax.swing.table.DefaultTableCellRenderer
import de.sciss.lucre.stm
import collection.breakOut
import scala.concurrent.stm.TxnLocal
import scala.annotation.tailrec
import de.sciss.treetable.j.DefaultTreeTableCellEditor
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing._

object TreeTableViewImpl {
  //  import scala.swing.Graphics2D
  //  import javax.swing.Icon
  //  import java.awt.{EventQueue, RenderingHints, Color, Graphics}
  //  import java.awt.geom.GeneralPath
  //  object Icons {
  //    object AddItem extends Icon {
  //      def getIconWidth  = 16
  //      def getIconHeight = 16
  //
  //      def paintIcon(c: java.awt.Component, g: Graphics, x: Int, y: Int): Unit = {
  //        g.setColor(Color.black)
  //        g.fillRect(x    , y + 6, 16,  4)
  //        g.fillRect(x + 6, y    ,  4, 16)
  //      }
  //    }
  //
  //    object RemoveItem extends Icon {
  //      def getIconWidth  = 16
  //      def getIconHeight = 16
  //
  //      def paintIcon(c: java.awt.Component, g: Graphics, x: Int, y: Int): Unit = {
  //        g.setColor(Color.black)
  //        g.fillRect(x, y + 6, 16, 4)
  //      }
  //    }
  //
  //    object ViewItem extends Icon {
  //      def getIconWidth  = 16
  //      def getIconHeight = 16
  //
  //      private val shape = new GeneralPath
  //      shape.moveTo(0, 8)
  //      shape.quadTo(8,  0, 16, 8)
  //      shape.quadTo(8, 16,  0, 8)
  //      shape.closePath()
  //
  //      def paintIcon(c: java.awt.Component, g: Graphics, x: Int, y: Int): Unit = {
  //        val g2      = g.asInstanceOf[Graphics2D]
  //        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
  //        val atOrig  = g2.getTransform
  //        g2.setColor(Color.black)
  //        g2.translate(x, y)
  //        g2.fill(shape)
  //        g2.setTransform(atOrig)
  //      }
  //    }
  //  }

  private final val DEBUG = false

  private object NodeView {
    // sealed trait OrRoot[S <: Sys[S], T <: TreeLike[S, T], H <: Handler[S, T, H]]

    //    sealed trait BranchOrRoot[S <: Sys[S], T <: TreeLike[S, T], H <: Handler[S, T, H]] extends OrRoot[S, T, H] {
    //      /** The children of the folder. This variable _must only be accessed or updated_ on the event thread. */
    //      var children = Vec.empty[NodeView[S, T, H]]
    //      var renderData: H#BD
    //    }

    class Branch[S <: Sys[S], T <: TreeLike[S, T], H <: Handler[S, T, H]](val parentOption: Option[Branch[S, T, H]],
                                                                          var renderData: H#BD,
                                                                          src: stm.Source[S#Tx, T#Branch])
      extends /* BranchOrRoot[S, T, H] with */ NodeView[S, T, H] {

      var children = Vec.empty[NodeView[S, T, H]]
      val modelData = stm.Source.map(src)(TreeLike.IsBranch.apply)
      def isLeaf = false

      override def toString = s"Branch($src, $renderData)"
    }

    //    class Root[S <: Sys[S], T <: TreeLike[S, T], H <: Handler[S, T, H]](var renderData: H#BD)
    //      extends BranchOrRoot[S, T, H]

    class Leaf[S <: Sys[S], T <: TreeLike[S, T], H <: Handler[S, T, H]](val parent: Branch[S, T, H],
                                                                        var renderData: H#LD,
                                                                        src: stm.Source[S#Tx, T#Leaf])
      extends NodeView[S, T, H] {

      def parentOption = Some(parent)
      // def modelData()(implicit tx: S#Tx) = TreeLike.IsLeaf(src())
      val modelData = stm.Source.map(src)(TreeLike.IsLeaf.apply)
      def isLeaf = true

      override def toString = s"Leaf($src, $renderData)"
    }
  }
  private sealed trait NodeView[S <: Sys[S], T <: TreeLike[S, T], H <: Handler[S, T, H]]
    extends /* NodeView.OrRoot[S, T, H] with */ TreeTableView.Node[S, T, H] {

    // def renderData: H#D
    def parentOption: Option[NodeView.Branch[S, T, H]] // NodeView.BranchOrRoot[S, T, H]
    // def modelData()(implicit tx: S#Tx): TreeLike.Node[T#Branch, T#Leaf]
  }

  def apply[S <: Sys[S], T <: TreeLike[S, T], H <: Handler[S, T, H]](tree: T, handler: H)
                                                                    (implicit tx: S#Tx): TreeTableView[S, T, H] = {
    type Branch = T#Branch
    val _handler = handler
    val root: T#Branch = tree.root
    val _tree   = tree
    new Impl[S, T, H] {
      val mapViews  = tx.newInMemoryIDMap[VNode]  // folder IDs to renderers
      // val rootView  = new NodeView.Root[S, T, H](_handler.branchData(root))
      val rootView  = new NodeView.Branch[S, T, H](parentOption = None, renderData = _handler.branchData(root),
        src = tx.newHandle(root)(_tree.branchSerializer))
      mapViews.put(_handler.branchID(root), rootView)
      val handler   = _handler
      val tree      = _tree

      private def buildMapView(f: Branch, fv: NodeView.Branch[S, T, H]): Unit = {
        val vs = f.iterator.map { c =>
          addElem(fv, -1, c, refresh = false) { (b, bv) =>
            buildMapView(b, bv)
          }
        }.toIndexedSeq
        fv.children = vs
      }
      buildMapView(root, rootView)

      val observer: Disposable[S#Tx] = tree.changed.react { implicit tx => upd =>
        // println(s"UPDATE:\n$upd")
        folderUpdated(rootView, upd.branch)
      }

      deferTx {
        guiInit()
      }
    }
  }

  private abstract class Impl[S <: Sys[S], T <: TreeLike[S, T], H <: Handler[S, T, H]]
    extends ComponentHolder[Component] with TreeTableView[S, T, H] with ModelImpl[TreeTableView.Update] {
    view =>

    // type V        = NodeView.OrRoot       [S, T, H]
    type VBranch  = NodeView.Branch       [S, T, H]
    type VLeaf    = NodeView.Leaf         [S, T, H]
    type VNode    = NodeView              [S, T, H]
    type TPath    = TreeTable.Path[VBranch]
    type TNode    = TreeLike.Node[T#Branch, T#Leaf]
    type Node     = VNode // alias in the interface

    protected def rootView: NodeView[S, T, H] // .Root [S, T, H]
    protected def mapViews: IdentifierMap[S#ID, S#Tx, VNode]
    protected def observer: Disposable[S#Tx]
    protected def handler : H
    protected def tree    : T

    private val didInsert = TxnLocal(false)

    private class ElementTreeModel extends AbstractTreeModel[VNode] {
      lazy val root: VNode = rootView // ! must be lazy. suckers....

      def getChildCount(parent: VNode): Int = parent match {
        case b: VBranch => b.children.size
        case _          => 0
      }

      def getChild(parent: VNode, index: Int): VNode = parent match {
        case b: VBranch => b.children(index)
        case _          => sys.error(s"parent $parent is not a branch")
      }

      def isLeaf(node: VNode): Boolean = node.isLeaf

      def getIndexOfChild(parent: VNode, child: VNode): Int = parent match {
        case b: VBranch => b.children.indexOf(child)
        case _          => sys.error(s"parent $parent is not a branch")
      }

      def getParent(node: VNode): Option[VNode] = node.parentOption

      def getPath(node: VNode): TreeTable.Path[VNode] = {
        @tailrec def loop(n: VNode, res: TreeTable.Path[VNode]): TreeTable.Path[VNode] = {
          val res1 = n +: res
          n.parentOption match {
            case Some(p) => loop(p, res1)
            case _ => res1
          }
        }
        loop(node, Vec.empty)
      }

      def valueForPathChanged(path: TreeTable.Path[VNode], newValue: VNode): Unit =
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
        // this is insane. the tree UI still accesses the model based on the previous assumption
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
    private var t: TreeTable[VNode, TreeColumnModel[VNode]] = _

    def treeTable: TreeTable[VNode, _] = t

    def selection: List[VNode] = t.selection.paths.flatMap(_.lastOption)(breakOut)

    def markInsertion()(implicit tx: S#Tx): Unit = didInsert.update(true)(tx.peer)

    def addElem(parent: VBranch, idx: Int, elem: TNode, refresh: Boolean, edit: Boolean = false)
               (branch: (T#Branch, VBranch) => Unit)(implicit tx: S#Tx): VNode = {
      def addView(id: S#ID, v: VNode): Unit = {
        mapViews.put(id, v)

        if (refresh) deferTx {
          _model.elemAdded(parent, idx, v)
          if (edit) {
            //            val sel = t.selection.paths
            //            sel.clear()
            val path = _model.getPath(v)
            //            sel += path
            //            EventQueue.invokeLater(new Runnable {
            //              def run(): Unit =
            // t.startEditingAtPath(path)
            val row     = t.getRowForPath(path)
            val column  = t.hierarchicalColumn
            t.requestFocus()
            //            val tj  = t.peer
            //            val tje = tj.getCellEditor
            //            tje.shouldSelectCell(new javax.swing.event.ListSelectionEvent(tj, row, row, true))
            t.changeSelection(row, column, toggle = false, extend = false)
            t.editCellAt     (row, column)

            // TODO: this doesn't work yet, it doesn't activate the cursor, see TreeTable issue #12

            //            val tej = t.peer.getEditorComponent
            //            EventQueue.invokeLater(new Runnable {
            //              def run(): Unit = tej.requestFocus()
            //            })
            //            tej match {
            //              case tf: javax.swing.JTextField => ...
            //              case _ =>
            //            }

            // })
            // t.editCellAt(row, column)
          }
        }
      }

      elem match {
        case IsLeaf(l) =>
          val data  = handler.leafData(l)
          val id    = handler.leafID(l)
          val src   = tx.newHandle(l)(tree.leafSerializer)
          val _v    = new NodeView.Leaf[S, T, H](parent, data, src)
          addView(id, _v)
          _v

        case IsBranch(b) =>
          val data  = handler.branchData(b)
          val id    = handler.branchID(b)
          val src   = tx.newHandle(b)(tree.branchSerializer)
          val _v    = new NodeView.Branch[S, T, H](Some(parent), data, src)
          addView(id, _v)
          branch(b, _v)
          // buildMapView(b, _v)
          _v
      }
    }

    def elemAdded(parent: VBranch, idx: Int, elem: TNode)(implicit tx: S#Tx): Unit = {
      val edit = didInsert.swap(false)(tx.peer)

      if (DEBUG) println(s"elemAdded($parent, $idx $elem); marked? $edit")

      addElem(parent, idx, elem, refresh = true, edit = edit) { (b, bv) =>
        if (b.nonEmpty) b.iterator.toList.zipWithIndex.foreach { case (c, ci) =>
          elemAdded(bv, ci, c)
        }
      }
    }

    def elemRemoved(parent: VBranch, idx: Int, elem: TNode)(implicit tx: S#Tx): Unit = {
      if (DEBUG) println(s"elemRemoved($parent, $idx, $elem)")

      def removeView(id: S#ID): Unit = {
        mapViews.remove(id)
        deferTx {
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
            case Some(fv: VBranch) =>
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
          val oldData = v.renderData
          val newData = handler.branchUpdate(b, upd, oldData)
          if (newData != oldData) deferTx {
            v.renderData = newData
            _model.elemUpdated(v)
          }
        case other => warnNoBranchView(b, other)
      }
    }

    def leafElemUpdated(l: T#Leaf, upd: T#LU)(implicit tx: S#Tx): Unit = {
      val id = handler.leafID(l)
      mapViews.get(id) match {
        case Some(v: VLeaf) =>
          val oldData = v.renderData
          val newData = handler.leafUpdate(l, upd, oldData)
          if (newData != oldData) deferTx {
            v.renderData = newData
            _model.elemUpdated(v)
          }
        case other => warnNoLeafView(l, other)
      }
    }

    def insertionPoint()(implicit tx: S#Tx): (T#Branch, Int) = {
      val pOpt = treeTable.selection.paths.headOption.flatMap {
        case path @ init :+ last =>
          last.modelData() match {
            case TreeLike.IsBranch(b) if treeTable.isExpanded(path) => Some(b -> 0)
            case child => init match {
              case _ :+ _parent =>
                _parent.modelData() match {
                  case TreeLike.IsBranch(b) => Some(b -> (b.indexOf(child) + 1))
                  case _ => None
                }

              case _ => None
            }
          }
        case _ => None
      }
      pOpt.getOrElse(tree.root -> tree.root.size)
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
            case Some(vParent1: VBranch) => folderUpdated(vParent1, upd1)
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

      val tcm = new TreeColumnModel[VNode] {
        private val peer = handler.columns

        def getValueAt(r: VNode, column: Int): Any = r

        //        match {
        //          case node: VNode  => peer.getValueAt(node.data, column)
        //          case _            => println(s"----1---- $r"); null // XXX correct?
        //        }

        def setValueAt(value: Any, r: VNode, column: Int): Unit = r match {
          case node: VNode  => peer.setValueAt(value, node, column)
          case _            => throw new IllegalStateException(s"Trying to alter $r")
        }

        def getColumnName (column: Int): String   = peer.getColumnName (column)
        def getColumnClass(column: Int): Class[_] = classOf[AnyRef] // classOf[V] // peer.getColumnClass(column)

        def columnCount: Int = peer.columnCount

        def isCellEditable(r: VNode, column: Int): Boolean = r match {
          case node: VNode  => peer.isCellEditable(node, column)
          case _            => false
        }

        def hierarchicalColumn: Int = peer.hierarchicalColumn
      }

      t = new TreeTable(_model, tcm: TreeColumnModel[VNode])
      t.rootVisible = false
      val r = new DefaultTableCellRenderer with TreeTableCellRenderer {
        // private lazy val lb = new Label
        private lazy val wrapSelf = Component.wrap(this)

        def getRendererComponent(treeTable: TreeTable[_, _], value: Any, row: Int, column: Int,
                                 state: TreeTableCellRenderer.State): Component = {
          value match {
            case b: VBranch  =>
              // println(s"branchRenderer(${b.data}, row = $row)")
              handler.branchRenderer(view, b.renderData, row = row, column = column, state = state)
            case l: VLeaf     =>
              // println(s"leafRenderer(${l.data}, row = $row)")
              handler.leafRenderer  (view, l.renderData, row = row, column = column, state = state)
            case _ =>
              // println(s"----2---- ${if (value == null) "null" else value.getClass}")
              // lb // null
              wrapSelf
          }
        }
      }

      val rj = new DefaultTableCellRenderer with j.TreeTableCellRenderer {
        def getTreeTableCellRendererComponent(treeTable: j.TreeTable, value: Any, selected: Boolean,
                                              hasFocus: Boolean, row: Int, column: Int): java.awt.Component = {
          val state = TreeTableCellRenderer.State(selected = selected, focused = hasFocus, tree = None)
          r.getRendererComponent(t, value, row, column, state).peer
        }

        def getTreeTableCellRendererComponent(treeTable: j.TreeTable, value: Any, selected: Boolean,
                                              hasFocus: Boolean, row: Int, column: Int,
                                              expanded: Boolean, leaf: Boolean): java.awt.Component = {
          val treeState = TreeTableCellRenderer.TreeState(expanded = expanded, leaf = leaf)
          val state = TreeTableCellRenderer.State(selected = selected, focused = hasFocus, tree = Some(treeState))
          r.getRendererComponent(t, value, row, column, state).peer
        }
      }

      val ej = new DefaultTreeTableCellEditor(new javax.swing.JTextField()) {
        override def getTreeTableCellEditorComponent(treeTable: j.TreeTable, value: Any, selected: Boolean,
                                                     row: Int, column: Int): java.awt.Component = {
          val v1 = value match {
            case b: VBranch  =>
              // println(s"branchRenderer(${b.data}, row = $row)")
              b.renderData
            case l: VLeaf     =>
              // println(s"leafRenderer(${l.data}, row = $row)")
              l.renderData
            case _ => value
          }
          super.getTreeTableCellEditorComponent(treeTable, v1, selected, row, column)
        }

        override def getTreeTableCellEditorComponent(treeTable: j.TreeTable, value: Any, selected: Boolean,
                                                     row: Int, column: Int, expanded: Boolean,
                                                     leaf: Boolean): java.awt.Component = {
          val v1 = value match {
            case b: VBranch  =>
              // println(s"branchRenderer(${b.data}, row = $row)")
              b.renderData
            case l: VLeaf     =>
              // println(s"leafRenderer(${l.data}, row = $row)")
              l.renderData
            case _ => value
          }
          super.getTreeTableCellEditorComponent(treeTable, v1, selected, row, column, expanded, leaf)
        }
      }

      // t.renderer = r

      //      val r1 = new DefaultTableCellRenderer with TreeTableCellRenderer {
      //        def getRendererComponent(treeTable: TreeTable[_, _], value: Any, row: Int, column: Int, state: State) = ??? : Component
      //
      //        override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean,
      //                                                   row: Int, column: Int): java.awt.Component = {
      //          val state = TreeTableCellRenderer.State(selected = isSelected, focused = hasFocus, tree = None)
      //          value match {
      //            case b: VBranch  =>
      //              // println(s"branchRenderer(${b.data}, row = $row)")
      //              handler.branchRenderer(view, b.data, row = row, column = column, state = state).peer
      //            case l: VLeaf     =>
      //              // println(s"leafRenderer(${l.data}, row = $row)")
      //              handler.leafRenderer  (view, l.data, row = row, column = column, state = state).peer
      //            case _ =>
      //              // println(s"----2---- ${if (value == null) "null" else value.getClass}")
      //              super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
      //          }
      //        }
      //      }

      val cm = t.peer.getColumnModel
      for (col <- 0 until handler.columns.columnCount) {
        // assert(r.isInstanceOf[TreeTableCellRenderer])
        val c = cm.getColumn(col)
        c.setCellRenderer(rj)
        c.setCellEditor  (ej)
      }

      //      val tabCM = t.peer.getColumnModel
      //      tabCM.getColumn(0).setPreferredWidth(176)
      //      tabCM.getColumn(1).setPreferredWidth(256)

      t.listenTo(t.selection)
      t.reactions += {
        case e: TreeTableSelectionChanged[_, _] =>  // this crappy untyped event doesn't help us at all
          dispatch(TreeTableView.SelectionChanged)
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
    //        case PathExtractor(path, child) => (path, child)
    //      })(breakOut)
    //
    //    object PathExtractor {
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
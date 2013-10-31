package at.iem.sysson
package gui
package impl

import de.sciss.synth.proc.ExprImplicits
import swing.{ScrollPane, Component}
import scala.collection.breakOut
import de.sciss.lucre.stm.{Disposable, Cursor, IdentifierMap}
import de.sciss.model.impl.ModelImpl
import scala.util.control.NonFatal
import javax.swing.{Icon, DropMode}
import de.sciss.treetable.{TreeTableSelectionChanged, TreeTableCellRenderer, TreeColumnModel, AbstractTreeModel, TreeTable}
import de.sciss.treetable.TreeTableCellRenderer.TreeState
import de.sciss.model.Change
import GUI.{fromTx => guiFromTx, requireEDT}
import de.sciss.lucre.event.Sys
import TreeLike.{IsBranch, IsLeaf}

object TreeTableViewImpl {
  private final val DEBUG = false

  private object NodeView {
    def apply[S <: Sys[S], T <: TreeLike[S, _, _, T]](parent: FolderLike[S, T],
                                                      child: TreeLike.Node[T#Branch, T#Leaf]): NodeView[S, T] = ???

    sealed trait Renderer[S <: Sys[S], T <: TreeLike[S, _, _, T]] {
      // private[gui] def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component
      def name: String
      def parent: Option[NodeView.FolderLike[S, T]]
      def value: Any
      def tryUpdate(value: Any)(implicit tx: S#Tx): Boolean
      def icon: Icon
    }

    sealed trait FolderLike[S <: Sys[S], T <: TreeLike[S, _, _, T]] extends Renderer[S, T] {
      // def branchID(implicit tx: S#Tx) = folder.id

      def folder(implicit tx: S#Tx): T#Branch

      //      def tryConvert(upd: Any): Folder.Update[S] = upd match {
      //        case ll: LinkedList.Update[_, _, _] =>
      //          convert(ll.asInstanceOf[LinkedList.Update[S, Element[S], Element.Update[S]]])
      //        case _ => Vector.empty
      //      }

      //      def convert(upd: LinkedList.Update[S, Element[S], Element.Update[S]]): _Folder.Update[S] =
      //        upd.changes.map {
      //          case LinkedList.Added  (idx, elem)  => _Folder.Added  (idx, elem)
      //          case LinkedList.Removed(idx, elem)  => _Folder.Removed(idx, elem)
      //          case LinkedList.Element(elem, eu)   => _Folder.Element(elem, eu)
      //        }

      /** The children of the folder. This variable _must only be accessed or updated_ on the event thread. */
      var children: Vec[NodeView[S, T]]

      def value = ()
    }

    sealed trait Folder[S <: Sys[S], T <: TreeLike[S, _, _, T]] extends FolderLike[S, T] with NodeView[S, T]

    object Root {
      def apply[S <: Sys[S], T <: TreeLike[S, _, _, T]](folder: T#Branch)(implicit tx: S#Tx): Root[S, T] = ???
    }
    sealed trait Root[S <: Sys[S], T <: TreeLike[S, _, _, T]] extends FolderLike[S, T]
  }
  private sealed trait NodeView[S <: Sys[S], T <: TreeLike[S, _, _, T]] extends NodeView.Renderer[S, T]
  private trait ChildView [S <: Sys[S], T <: TreeLike[S, _, _, T]] extends NodeView[S, T]
  private trait BranchView[S <: Sys[S], T <: TreeLike[S, _, _, T]] extends NodeView[S, T]

  def apply[S <: Sys[S], T <: TreeLike[S, _, _, T]](tree: T, config: TreeTableView.Config[S, T])
                        (implicit tx: S#Tx, cursor: Cursor[S]): TreeTableView[S, T] = {

    type Folder = T#Branch

    // val _doc    = document
    val _cursor = cursor
    val _config = config
    val root: T#Branch = tree.root
    new Impl[S, T] {
      val cursor    = _cursor
      val mapViews  = tx.newInMemoryIDMap[NodeView[S, T]]  // folder IDs to renderers
      val rootView  = NodeView.Root[S, T](root)
      val config    = _config
      // val document  = _doc

      private def buildMapView(f: Folder, fv: NodeView.FolderLike[S, T]): Unit = ???
//      {
//        val tup = f.iterator.map(c => c -> NodeView(fv, c)(tx)).toIndexedSeq
//        fv.children = tup.map(_._2)
//        tup.foreach { case (c, cv) =>
//          mapViews.put(c.id, cv)
//          (c, cv) match {
//            case (cf: Element.Folder, cfv: NodeView.Folder) =>
//              buildMapView(cf.entity, cfv)
//            case _ =>
//          }
//        }
//      }
      buildMapView(root, rootView)

      val observer = ???
//      tree.changed.react { implicit tx => upd =>
//        // val c = rootView.convert(upd)
//        // folderUpdated(rootView, c)
//        ()
//      }

      guiFromTx {
        guiInit()
      }
    }
  }

  private abstract class Impl[S <: Sys[S], T <: TreeLike[S, _, _, T]]
    extends ComponentHolder[Component] with TreeTableView[S, T] with ModelImpl[Any /* FolderView.Update[S] */] {
    view =>

    type Node   = NodeView.Renderer[S, T]
    type Branch = NodeView.FolderLike[S, T]
    type Path   = TreeTable.Path[Branch]
    type TNode  = TreeLike.Node[T#Branch, T#Leaf]

    protected def rootView: NodeView.Root[S, T]
    protected def mapViews: IdentifierMap[S#ID, S#Tx, NodeView[S, T]]
    protected implicit def cursor: Cursor[S]
    protected def observer: Disposable[S#Tx]
    // protected def document: Document[S]
    protected def config: TreeTableView.Config[S, T]

    private class ElementTreeModel extends AbstractTreeModel[Node] {
      lazy val root: Node = rootView // ! must be lazy. suckers....

      def getChildCount(parent: Node): Int = parent match {
        case b: NodeView.FolderLike[S, T] => b.children.size
        case _ => 0
      }

      def getChild(parent: Node, index: Int): NodeView[S, T] = parent match {
        case b: NodeView.FolderLike[S, T] => b.children(index)
        case _ => sys.error(s"parent $parent is not a branch")
      }

      def isLeaf(node: Node): Boolean = node match {
        case b: NodeView.FolderLike[S, T] => false // b.children.nonEmpty
        case _ => true
      }

      def getIndexOfChild(parent: Node, child: Node): Int = parent match {
        case b: NodeView.FolderLike[S, T] => b.children.indexOf(child)
        case _ => sys.error(s"parent $parent is not a branch")
      }

      def getParent(node: Node): Option[Node] = node.parent

      def valueForPathChanged(path: TreeTable.Path[Node], newValue: Node): Unit =
        println(s"valueForPathChanged($path, $newValue)")

      def elemAdded(parent: NodeView.FolderLike[S, T], idx: Int, view: NodeView[S, T]): Unit = {
        if (DEBUG) println(s"model.elemAdded($parent, $idx, $view)")
        val g       = parent  // Option.getOrElse(_root)
        require(idx >= 0 && idx <= g.children.size)
        g.children  = g.children.patch(idx, Vector(view), 0)
        fireNodesInserted(view)
      }

      def elemRemoved(parent: NodeView.FolderLike[S, T], idx: Int): Unit = {
        if (DEBUG) println(s"model.elemRemoved($parent, $idx)")
        require(idx >= 0 && idx < parent.children.size)
        val v       = parent.children(idx)
        // this is frickin insane. the tree UI still accesses the model based on the previous assumption
        // about the number of children, it seems. therefore, we must not update children before
        // returning from fireNodesRemoved.
        fireNodesRemoved(v)
        parent.children  = parent.children.patch(idx, Vector.empty, 1)
      }

      def elemUpdated(view: NodeView[S, T]): Unit = {
        if (DEBUG) println(s"model.elemUpdated($view)")
        fireNodesChanged(view)
      }
    }

    private var _model: ElementTreeModel  = _
    private var t: TreeTable[Node, TreeColumnModel[Node]] = _

    def elemAdded(parent: NodeView.FolderLike[S, T], idx: Int, elem: TNode)(implicit tx: S#Tx): Unit = {
      if (DEBUG) println(s"elemAdded($parent, $idx $elem)")
      val v   = NodeView(parent, elem)
      val id  = config.idView(elem)
      mapViews.put(id, v)

      guiFromTx {
        _model.elemAdded(parent, idx, v)
      }

      (elem, v) match {
        case (IsBranch(f), fv: NodeView.Folder[S, T]) =>
          val fe    = f // .entity
          // val path  = parent :+ gv
          // branchAdded(path, gv)
          if (!fe.isEmpty) {
            fe.iterator.toList.zipWithIndex.foreach { case (c, ci) =>
              elemAdded(fv, ci, c)
            }
          }

        case _ =>
      }
    }

    def elemRemoved(parent: NodeView.FolderLike[S, T], idx: Int, elem: TNode)(implicit tx: S#Tx): Unit = {
      if (DEBUG) println(s"elemRemoved($parent, $idx, $elem)")
      val id  = config.idView(elem)
      mapViews.get(id).foreach { v =>
        (elem, v) match {
          case (IsBranch(f), fv: NodeView.Folder[S, T]) =>
            // val path = parent :+ gl
            val fe = f // .entity
            if (fe.nonEmpty) fe.iterator.toList.zipWithIndex.reverse.foreach { case (c, ci) =>
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

    def elemUpdated(elem: TNode, changes: Vec[Any /* Element.Change[S] */])(implicit tx: S#Tx): Unit = {
      val id      = config.idView(elem)
      val viewOpt = mapViews.get(id)
      if (viewOpt.isEmpty) {
        println(s"WARNING: No view for elem $elem")
      }
//      viewOpt.foreach { v =>
//        changes.foreach {
//          case Element.Renamed(Change(_, newName)) =>
//            guiFromTx {
//              v.name = newName
//              _model.elemUpdated(v)
//            }
//
//          case Element.Entity(ch) =>
//            v match {
//              case fv: NodeView.Folder =>
//                val upd = fv.tryConvert(ch)
//                if (upd.isEmpty) println(s"WARNING: unhandled $elem -> $ch")
//                folderUpdated(fv, upd)
//
//              case _ =>
//                if (v.checkUpdate(ch)) guiFromTx(_model.elemUpdated(v))
//            }
//        }
//      }
    }

//    def folderUpdated(fv: NodeView.FolderLike[S, T], upd: Folder.Update[S])(implicit tx: S#Tx): Unit =
//      upd.foreach {
//        case Folder.Added  (idx, elem)      => elemAdded  (fv, idx, elem)
//        case Folder.Removed(idx, elem)      => elemRemoved(fv, idx, elem)
//        case Folder.Element(elem, elemUpd)  => elemUpdated(elem, elemUpd.changes)
//      }

    def dispose()(implicit tx: S#Tx): Unit = {
      observer.dispose()
      mapViews.dispose()
    }

    protected def guiInit(): Unit = {
      requireEDT()

      _model = new ElementTreeModel

      val colName = new TreeColumnModel.Column[Node, String]("Name") {
        def apply(node: Node): String = node.name

        def update(node: Node, value: String): Unit = ???
//          node match {
//            case v: NodeView[S, T] if value != v.name =>
//              cursor.step { implicit tx =>
//                val expr = ExprImplicits[S]
//                import expr._
//                v.element().name() = value
//              }
//            case _ =>
//          }

        def isEditable(node: Node) = node match {
          case b: NodeView[S, T] => true
          case _ => false // i.e. Root
        }
      }

      val colValue = new TreeColumnModel.Column[Node, Any]("Value") {
        def apply(node: Node): Any = node.value

        def update(node: Node, value: Any): Unit =
          cursor.step { implicit tx => node.tryUpdate(value) }

        def isEditable(node: Node) = node match {
          case b: NodeView.FolderLike[S, T] => false
          case _ => true
        }
      }

      val tcm = new TreeColumnModel.Tuple2[Node, String, Any](colName, colValue) {
        def getParent(node: Node): Option[Node] = node.parent
      }

      t = new TreeTable(_model, tcm)
      t.rootVisible = false
      t.renderer    = new TreeTableCellRenderer {
        private val component = TreeTableCellRenderer.Default
        def getRendererComponent(treeTable: TreeTable[_, _], value: Any, row: Int, column: Int,
                                 state: TreeTableCellRenderer.State): Component = {
          val value1 = if (value != ()) value else null
          val res = component.getRendererComponent(treeTable, value1, row = row, column = column, state = state)
          if (row >= 0) state.tree match {
            case Some(TreeState(false, true)) =>
              // println(s"row = $row, col = $column")
              try {
                val node = t.getNode(row)
                component.icon = node.icon
              } catch {
                case NonFatal(_) => // XXX TODO -- currently NPE probs; seems renderer is called before tree expansion with node missing
              }
            case _ =>
          }
          res // component
        }
      }
      val tabCM = t.peer.getColumnModel
      tabCM.getColumn(0).setPreferredWidth(176)
      tabCM.getColumn(1).setPreferredWidth(256)

      t.listenTo(t.selection)
      t.reactions += {
        case e: TreeTableSelectionChanged[_, _] =>  // this crappy untyped event doesn't help us at all
          // println(s"selection: $e")
          // dispatch(FolderView.SelectionChanged(view, selection))
        // case e => println(s"other: $e")
      }
      t.showsRootHandles  = true
      t.expandPath(TreeTable.Path(_model.root))
      t.dragEnabled       = true
      t.dropMode          = DropMode.ON_OR_INSERT_ROWS
      //      t.peer.setTransferHandler(new TransferHandler {
      //        // ---- export ----
      //
      //        override def getSourceActions(c: JComponent): Int =
      //          TransferHandler.COPY | TransferHandler.MOVE // dragging only works when MOVE is included. Why?
      //
      //        override def createTransferable(c: JComponent): Transferable = {
      //          val sel   = selection
      //          val tSel  = DragAndDrop.Transferable(FolderView.selectionFlavor) {
      //            new FolderView.SelectionDnDData(document, selection)
      //          }
      //          // except for the general selection flavour, see if there is more specific types
      //          // (current Int and Code are supported)
      //          sel.headOption match {
      //            case Some((_, elemView: NodeView.Int[S])) =>
      //              val elem  = elemView.element
      //              val tElem = DragAndDrop.Transferable(timeline.DnD.flavor) {
      //                timeline.DnD.IntDrag[S](document, elem)
      //              }
      //              DragAndDrop.Transferable.seq(tSel, tElem)
      //
      //            case Some((_, elemView: NodeView.Code[S])) /* if elemView.value.id == Code.SynthGraph.id */ =>
      //              val elem  = elemView.element
      //              val tElem = DragAndDrop.Transferable(timeline.DnD.flavor) {
      //                timeline.DnD.CodeDrag[S](document, elem)
      //              }
      //              DragAndDrop.Transferable.seq(tSel, tElem)
      //
      //            case _ => tSel
      //          }
      //        }
      //
      //        // ---- import ----
      //        override def canImport(support: TransferSupport): Boolean =
      //          t.dropLocation match {
      //            case Some(tdl) =>
      //              // println(s"last = ${tdl.path.last}; column ${tdl.column}; isLeaf? ${t.treeModel.isLeaf(tdl.path.last)}")
      //              val locOk = tdl.index >= 0 || (tdl.column == 0 && (tdl.path.last match {
      //                case _: NodeView.Folder[_] => true
      //                case _                        => false
      //              }))
      //
      //              if (locOk) {
      //                // println("Supported flavours:")
      //                // support.getDataFlavors.foreach(println)
      //
      //                // println(s"File? ${support.isDataFlavorSupported(java.awt.datatransfer.DataFlavor.javaFileListFlavor)}")
      //                // println(s"Action = ${support.getUserDropAction}")
      //
      //                support.isDataFlavorSupported(DataFlavor.javaFileListFlavor) ||
      //                  (support.isDataFlavorSupported(FolderView.selectionFlavor) &&
      //                   support.getUserDropAction == TransferHandler.MOVE)
      //
      //              } else {
      //                false
      //              }
      //
      //            case _ => false
      //          }
      //
      //        // XXX TODO: not sure whether removal should be in exportDone or something
      //        private def insertData(sel: FolderView.Selection[S], newParentView: Branch, idx: Int): Boolean = {
      //          // println(s"insert into $parent at index $idx")
      //
      //          def isNested(pv: Branch, cv: Branch): Boolean =
      //            pv == cv || pv.children.collect {
      //              case pcv: NodeView.Folder => pcv
      //            }.exists(isNested(_, cv))
      //
      //          // make sure we are not moving a folder within itself (it will magically disappear :)
      //          val sel1 = sel.filter {
      //            case (_, cv: NodeView.Folder) if isNested(cv, newParentView) => false
      //            case _ => true
      //          }
      //
      //          // if we move children within the same folder, adjust the insertion index by
      //          // decrementing it for any child which is above the insertion index, because
      //          // we will first remove all children, then re-insert them.
      //          val idx0 = if (idx >= 0) idx else newParentView.children.size
      //          val idx1 = idx0 - sel1.count {
      //            case (_ :+ `newParentView`, cv) => newParentView.children.indexOf(cv) <= idx0
      //            case _ => false
      //          }
      //          // println(s"idx0 $idx0 idx1 $idx1")
      //
      //          cursor.step { implicit tx =>
      //            val tup = sel1.map {
      //              case (_ :+ pv, cv) => pv.folder -> cv.element()
      //            }
      //
      //            val newParent = newParentView.folder
      //            tup             .foreach { case  (oldParent, c)       => oldParent.remove(            c) }
      //            tup.zipWithIndex.foreach { case ((_        , c), off) => newParent.insert(idx1 + off, c) }
      //          }
      //
      //          true
      //        }
      //
      //        private def importSelection(support: TransferSupport, parent: NodeView.FolderLike[S], index: Int): Boolean = {
      //          val data = support.getTransferable.getTransferData(FolderView.selectionFlavor)
      //            .asInstanceOf[FolderView.SelectionDnDData[S]]
      //          if (data.document == document) {
      //            val sel = data.selection
      //            insertData(sel, parent, index)
      //          } else {
      //            false
      //          }
      //        }
      //
      //        private def importFiles(support: TransferSupport, parent: NodeView.FolderLike[S], index: Int): Boolean = {
      //          import JavaConversions._
      //          val data  = support.getTransferable.getTransferData(DataFlavor.javaFileListFlavor)
      //            .asInstanceOf[java.util.List[File]].toList
      //          val tup   = data.flatMap { f =>
      //            Try(AudioFile.readSpec(f)).toOption.map(f -> _)
      //          }
      //          val trip  = tup.flatMap { case (f, spec) =>
      //            findLocation(f).map { loc => (f, spec, loc) }
      //          }
      //
      //          if (trip.nonEmpty) {
      //            cursor.step { implicit tx =>
      //              trip.foreach {
      //                case (f, spec, locS) =>
      //                  val loc = locS()
      //                  loc.entity.modifiableOption.foreach { locM =>
      //                    ElementActions.addAudioFile(parent.folder, index, locM, f, spec)
      //                  }
      //              }
      //            }
      //            true
      //
      //          } else {
      //            false
      //          }
      //        }
      //
      //        override def importData(support: TransferSupport): Boolean =
      //          t.dropLocation match {
      //            case Some(tdl) =>
      //              tdl.path.last match {
      //                case parent: NodeView.FolderLike[S] =>
      //                  val idx = tdl.index
      //                  if      (support.isDataFlavorSupported(FolderView.selectionFlavor   ))
      //                    importSelection(support, parent, idx)
      //                  else if (support.isDataFlavorSupported(DataFlavor.javaFileListFlavor))
      //                    importFiles    (support, parent, idx)
      //                  else false
      //
      //                case _ => false
      //              }
      //
      //            case _ => false
      //          }
      //      })

      val scroll    = new ScrollPane(t)
      scroll.border = null
      component     = scroll
    }

//    def selection: FolderView.Selection[S] =
//      t.selection.paths.collect({
//        case PathExtrator(path, child) => (path, child)
//      })(breakOut)
//
//    object PathExtrator {
//      def unapply(path: Seq[Node]): Option[(Vec[NodeView.FolderLike[S]], NodeView[S])] =
//        path match {
//          case init :+ (last: NodeView[S]) =>
//            val pre: Vec[NodeView.FolderLike[S]] = init.map({
//              case g: NodeView.FolderLike[S] => g
//              case _ => return None
//            })(breakOut)
//            Some((/* _root +: */ pre, last))
//          case _ => None
//        }
//    }
  }
}
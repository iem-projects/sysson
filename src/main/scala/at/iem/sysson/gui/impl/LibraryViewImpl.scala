package at.iem.sysson
package gui
package impl

import de.sciss.lucre.event.Sys
import de.sciss.treetable.TreeTableCellRenderer.State
import de.sciss.treetable.TreeColumnModel
import scala.swing.{Action, Orientation, BoxPanel, FlowPanel, Swing, ScrollPane, Component}
import de.sciss.model.Change
import de.sciss.treetable.j.DefaultTreeTableCellRenderer
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.UndoManager
import java.awt.Graphics
import de.sciss.lucre.stm
import de.sciss.lucre.synth.expr.ExprImplicits
import de.sciss.lucre.expr.{Expr, String => StringEx}
import de.sciss.icons.raphael
import javax.swing.{JComponent, TransferHandler}
import java.awt.datatransfer.Transferable
import javax.swing.undo.{CannotUndoException, CannotRedoException, AbstractUndoableEdit}
import at.iem.sysson.gui.DragAndDrop.LibraryNodeDrag
import de.sciss.lucre.swing.edit.EditVar
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing._

object LibraryViewImpl {
  def apply[S <: Sys[S]](library: Library[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): LibraryView[S] = {
    val undoMgr   = new UndoManagerImpl {
      protected var dirty: Boolean = false
    }
    val handler   = new Handler[S](undoMgr)
    val libH      = tx.newHandle(library)
    val res       = new Impl[S](undoMgr, libH, TreeTableView[S, Library[S], Handler[S]](library, handler))
    deferTx(res.guiInit())
    res
  }

  private final class Impl[S <: Sys[S]](val undoManager: UndoManager,
                                        libraryH: stm.Source[S#Tx, Library[S]],
                                        treeView: TreeTableView[S, Library[S], Handler[S]])
                                       (implicit val cursor: stm.Cursor[S])
    extends LibraryView[S] with ComponentHolder[Component] {
    impl =>

    def library(implicit tx: S#Tx): Library[S] = libraryH()

    // direction: true = insert, false = remove
    private class EditNode protected(direction: Boolean,
                                     parentH: stm.Source[S#Tx, Library.Branch[S]],
                                     index: Int,
                                     childH: stm.Source[S#Tx, Library.NodeLike[S]])
      extends AbstractUndoableEdit {

      override def undo(): Unit = {
        super.undo()
        cursor.step { implicit tx =>
          val success = if (direction) remove() else insert()
          if (!success) throw new CannotUndoException()
        }
      }

      override def redo(): Unit = {
        super.redo()
        cursor.step { implicit tx => perform() }
      }

      override def die(): Unit = {
        val hasBeenDone = canUndo
        super.die()
        if (!hasBeenDone) {
          // XXX TODO: dispose()
        }
      }

      private def insert()(implicit tx: S#Tx): Boolean = {
        val parent  = parentH()
        if (parent.size >= index) {
          val child = childH()
          parent.insert(index, child)
          true
        } else false
      }

      private def remove()(implicit tx: S#Tx): Boolean = {
        val parent = parentH()
        if (parent.size > index) {
          parent.removeAt(index)
          true
        } else false
      }

      def perform()(implicit tx: S#Tx): Unit = {
        val success = if (direction) insert() else remove()
        if (!success) throw new CannotRedoException()
      }
    }

    private class EditInsertNode(nodeType: String,
                                 parentH: stm.Source[S#Tx, Library.Branch[S]],
                                 index: Int,
                                 childH: stm.Source[S#Tx, Library.NodeLike[S]])
      extends EditNode(true, parentH, index, childH) {

      override def getPresentationName = s"Insert $nodeType"
    }

    private class EditRemoveNode(nodeType: String,
                                 parentH: stm.Source[S#Tx, Library.Branch[S]],
                                 index: Int,
                                 childH: stm.Source[S#Tx, Library.NodeLike[S]])
      extends EditNode(false, parentH, index, childH) {

      override def getPresentationName = s"Remove $nodeType"
    }

    def dispose()(implicit tx: S#Tx) = ()

    def guiInit(): Unit = {
      val scroll = new ScrollPane(treeView.component)
      scroll.border = Swing.EmptyBorder

      // def add(gen: S#Tx => TreeLike.Node[Library.Branch[S], Library.Leaf[S]]) ...

      def actionAdd(name: String)(create: (S#Tx) => Library.NodeLike[S]): Unit = {
        val edit = cursor.step { implicit tx =>
          val (parent, idx) = treeView.insertionPoint()
          treeView.markInsertion()
          val childH  = tx.newHandle(create(tx))
          val _edit   = new EditInsertNode(name, tx.newHandle(parent), idx, childH)
          _edit.perform()
          _edit
        }
        undoManager.add(edit)
      }

      val actionAddBranch = new Action("Folder") {
        def apply(): Unit = {
          val expr = ExprImplicits[S]
          import expr._
          actionAdd("Folder") { implicit tx => Library.Branch("untitled folder") }
        }
      }
      val ggAddBranch = GUI.toolButton(actionAddBranch, raphael.Shapes.Plus)

      val actionAddLeaf = new Action("Patch") {
        def apply(): Unit = {
          val expr = ExprImplicits[S]
          import expr._
          actionAdd("Patch") { implicit tx => Library.Leaf("untitled patch", "// Sonification SynthGraph body here") }
        }
      }
      val ggAddLeaf = GUI.toolButton(actionAddLeaf, raphael.Shapes.Plus)

      val actionRemove = new Action(null) {
        enabled = false

        def apply(): Unit = {
          val sel = treeView.selection.flatMap(n => n.parentOption.map(_ -> n))
          if (sel.nonEmpty) {
            val editOpt = impl.cursor.step { implicit tx =>
              sel.flatMap { case (pv, cv) =>
                val TreeLike.IsBranch(pm) = pv.modelData()
                val cm                    = cv.modelData()
                val idx = pm.indexOf(cm)
                if (idx >= 0) {
                  val parentH   = tx.newHandle(pm)
                  val childH    = tx.newHandle(cm.merge)
                  val nodeType  = cm match {
                    case TreeLike.IsBranch(_) => "Folder"
                    case TreeLike.IsLeaf  (_) => "Patch"
                  }
                  val edit      = new EditRemoveNode(nodeType, parentH, idx, childH)
                  edit.perform()
                  Some(edit)
                } else None
              }
            }
            editOpt.foreach(undoManager.add)
          }
        }
      }
      val ggRemove = GUI.toolButton(actionRemove, raphael.Shapes.Minus, tooltip = "Delete Entry")

      val actionView = new Action(null) {
        enabled = false

        def apply(): Unit = treeView.selection match {
          case single :: Nil if single.isLeaf =>
            impl.cursor.step { implicit tx =>
              single.modelData() match {
                case TreeLike.IsLeaf(l) => PatchCodeWindow(l, undoManager)
                case _                  =>
              }
            }
          case _ =>
        }
      }
      val ggView = GUI.toolButton(actionView, raphael.Shapes.Edit, tooltip = "Edit Patch")

      treeView.addListener {
        case TreeTableView.SelectionChanged =>
          val sel = treeView.selection
          actionRemove.enabled = sel.nonEmpty
          actionView  .enabled = sel match {
            case single :: Nil if single.isLeaf => true
            case _                              => false
        }
      }

      treeView.treeTable.peer.setTransferHandler(new TransferHandler(null) {
        // ---- export ----

        override def getSourceActions(c: JComponent): Int =
          TransferHandler.LINK | TransferHandler.COPY | TransferHandler.MOVE // dragging only works when MOVE is included. Why?

        override def createTransferable(c: JComponent): Transferable = {
          val opt = treeView.selection.collectFirst {
            case n if n.isLeaf => n.modelData
          }

          val res = opt.map { patch =>
            val drag = new LibraryNodeDrag {
              type S1 = S
              val cursor: stm.Cursor[S] = impl.cursor
              val node: stm.Source[S#Tx, Either[Library.Branch[S], Library.Leaf[S]]] = patch
            }
            DragAndDrop.Transferable(DragAndDrop.LibraryNodeFlavor)(drag)
          } .orNull
          // println(s"createTransferable: $res")
          res
        }
      })

      val flow  = new FlowPanel(ggAddBranch, ggAddLeaf, ggRemove, ggView)
      val panel = new BoxPanel(Orientation.Vertical) {
        contents += scroll
        contents += flow
      }

      component = panel
    }
  }

  //  private final class NameView(var name: String) {
  //    override def toString = name
  //  }

  private final class Handler[S <: Sys[S]](undo: UndoManager)(implicit cursor: stm.Cursor[S])
    extends TreeTableView.Handler[S, Library[S], Handler[S]] {

    type N  = TreeTableView.Node[S, Library[S], Handler[S]]

    type D  = String
    type BD = String
    type LD = String
    //    type D  = NameView
    //    type BD = NameView
    //    type LD = NameView

    def branchID(branch: Library.Branch[S]): S#ID = branch.id
    def leafID  (leaf  : Library.Leaf  [S]): S#ID = leaf  .id

    def branchData(branch: Library[S]#Branch)(implicit tx: S#Tx): BD = branch.name.value
    def leafData  (leaf  : Library[S]#Leaf  )(implicit tx: S#Tx): LD = leaf  .name.value

    private object rendererJ extends DefaultTreeTableCellRenderer {
      var selected  = false
      var viewJ     = null: de.sciss.treetable.j.TreeTable

      override def paintComponent(g: Graphics): Unit = {
        if (viewJ != null) {
          g.setColor(if(selected) viewJ.getSelectionBackground else viewJ.getBackground)
          g.fillRect(0, 0, getWidth, getHeight)
        }
        super.paintComponent(g)
      }
    }
    private val renderer  = Component.wrap(rendererJ)

    object columns extends TreeColumnModel[N] {
      def setValueAt(value: Any, node: N, column: Int): Unit = {
        // println(s"TODO: setValueAt($value, $node, $column)")
        require(column == 0)
        val editOpt = cursor.step { implicit tx =>
          node.modelData().merge.name match {
            case Expr.Var(v) =>
              val expr = ExprImplicits[S]
              import expr._
              import StringEx.{serializer, varSerializer}
              val s: Expr[S, String] = value.toString
              val edit = EditVar.Expr("Rename Node", v, value = s)
              Some(edit)

            case _ => None
          }
        }
        editOpt.foreach(undo.add)
      }

      def getColumnName(column: Int) = "Name"

      def getColumnClass(column: Int) = classOf[String]

      def columnCount = 1

      def getValueAt(node: N, column: Int) = node.renderData

      def isCellEditable(node: N, column: Int) = column == 0

      def hierarchicalColumn = 0
    }

    private def configRenderer(view: TreeTableView[S, Library[S], Handler[S]], data: BD, row: Int, column: Int,
                               state: State): Component = {
      val viewJ = view.treeTable.peer
      state.tree.fold[Unit] {
        rendererJ.getTreeTableCellRendererComponent(viewJ, data, state.selected, state.focused,
          row, column)
      } { tree =>
        rendererJ.getTreeTableCellRendererComponent(viewJ, data, state.selected, state.focused,
          row, column, tree.expanded, tree.leaf)
      }
      rendererJ.setText(data)
      // rendererJ.setBackground(Color.red)
      rendererJ.selected  = state.selected
      rendererJ.viewJ     = viewJ
      // rendererJ.setBackground(if(state.selected) viewJ.getSelectionBackground else viewJ.getBackground)
      renderer
    }

    def branchRenderer(view: TreeTableView[S, Library[S], Handler[S]], data: BD, row: Int, column: Int,
                       state: State): Component =
      configRenderer(view, data, row, column, state)

    def leafRenderer(view: TreeTableView[S, Library[S], Handler[S]], data: LD, row: Int, column: Int,
                     state: State): Component =
      configRenderer(view, data, row, column, state)

    def branchUpdate(branch: Library.Branch[S], update: Library.Renamed, data: BD)(implicit tx: S#Tx): BD =
      update.change.now

    def leafUpdate(leaf: Library.Leaf[S], update: Library.LeafUpdate, data: LD)(implicit tx: S#Tx): LD =
      (data /: update) {
        case (_, Library.Renamed(Change(_, now))) => now
        case (prev, _) => prev
      }
  }
}
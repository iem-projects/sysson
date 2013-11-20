package at.iem.sysson
package gui
package impl

import de.sciss.lucre.event.Sys
import de.sciss.treetable.TreeTableCellRenderer.State
import de.sciss.treetable.TreeColumnModel
import scala.swing.{Action, Orientation, BoxPanel, Button, FlowPanel, Swing, ScrollPane, Component}
import de.sciss.model.Change
import de.sciss.treetable.j.DefaultTreeTableCellRenderer
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window
import java.awt.Graphics
import de.sciss.lucre.stm
import de.sciss.lucre.synth.expr.ExprImplicits
import scala.concurrent.stm.{TxnLocal, Ref}
import de.sciss.lucre.expr.Expr

object LibraryViewImpl {
  def apply[S <: Sys[S]](library: Library[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): LibraryView[S] = {
    val handler   = new Handler[S]
    val libH      = tx.newHandle(library)
    val res       = new Impl(libH, TreeTableView[S, Library[S], Handler[S]](library, handler))
    GUI.fromTx(res.guiInit())
    res
  }

  private final class Impl[S <: Sys[S]](libraryH: stm.Source[S#Tx, Library[S]],
                                        treeView: TreeTableView[S, Library[S], Handler[S]])
                                       (implicit cursor: stm.Cursor[S])
    extends LibraryView[S] with ComponentHolder[Component] {
    impl =>

    def library(implicit tx: S#Tx): Library[S] = libraryH()

    def guiInit(): Unit = {
      val scroll = new ScrollPane(treeView.component)
      scroll.border = Swing.EmptyBorder

      // def add(gen: S#Tx => TreeLike.Node[Library.Branch[S], Library.Leaf[S]]) ...

      val actionAddBranch = new Action("Folder") {
        icon = TreeTableViewImpl.Icons.AddItem

        def apply(): Unit = cursor.step { implicit tx =>
          val (parent, idx) = treeView.insertionPoint()
          val expr = ExprImplicits[S]
          import expr._
          treeView.markInsertion()
          /* val b = */ parent.insertBranch(idx, "untitled folder")
        }
      }
      val ggAddBranch = new Button(actionAddBranch)

      val actionAddLeaf = new Action("Patch") {
        icon = TreeTableViewImpl.Icons.AddItem

        def apply(): Unit = cursor.step { implicit tx =>
          val (parent, idx) = treeView.insertionPoint()
          val expr = ExprImplicits[S]
          import expr._
          treeView.markInsertion()
          /* val l = */ parent.insertLeaf(idx, name = "untitled patch", source = "// Sonification Synth Graph body here")
        }
      }
      val ggAddLeaf = new Button(actionAddLeaf)

      val actionRemove = new Action(null) {
        enabled = false
        icon    = TreeTableViewImpl.Icons.RemoveItem

        def apply(): Unit = {
          val sel = treeView.selection.flatMap(n => n.parentOption.map(_ -> n))
          if (sel.nonEmpty) impl.cursor.step { implicit tx =>
            sel.foreach { case (pv, cv) =>
              val TreeLike.IsBranch(pm) = pv.modelData()
              val cm                    = cv.modelData()
              pm.remove(cm)
            }
          }
        }
      }
      val ggRemove = new Button(actionRemove)

      val actionView = new Action(null) {
        enabled = false

        icon = TreeTableViewImpl.Icons.ViewItem

        def apply(): Unit = treeView.selection match {
          case single :: Nil if single.isLeaf =>
            impl.cursor.step { implicit tx =>
              single.modelData() match {
                case TreeLike.IsLeaf(l) =>
                  val name  = l.name.value
                  val code  = l.source.value
                  PatchCodeFrameImpl(name, Code.SynthGraph(code)) { (newName, newCode) =>
                    impl.cursor.step { implicit tx =>
                      val expr    = ExprImplicits[S]
                      import expr._
                      l.name()    = newName
                      l.source()  = newCode
                    }
                  }

                case _ =>
              }
            }
          case _ =>
        }
      }
      val ggView = new Button(actionView)

      treeView.addListener {
        case TreeTableView.SelectionChanged =>
          val sel = treeView.selection
          actionRemove.enabled = sel.nonEmpty
          actionView  .enabled = sel match {
            case single :: Nil if single.isLeaf => true
            case _                              => false
        }
      }

      val flow  = new FlowPanel(ggAddBranch, ggAddLeaf, ggRemove, ggView)
      val panel = new BoxPanel(Orientation.Vertical) {
        contents += scroll
        contents += flow
      }

      component = panel

      val f = new WindowImpl {
        frame =>

        def style       = Window.Regular
        def handler     = SwingApplication.windowHandler

        title           = "Library"
        contents        = impl.component
        closeOperation  = Window.CloseDispose

        //        bindMenus(
        //          "file.save" -> saveAction,
        //          "edit.undo" -> undoManager.undoAction,
        //          "edit.redo" -> undoManager.redoAction
        //        )

        pack()
        GUI.placeWindow(this, 1f, 0.25f, 20)

        def setDirtyFlag(value: Boolean): Unit = dirty = value
      }
      f.front()
    }
  }

  //  private final class NameView(var name: String) {
  //    override def toString = name
  //  }

  private final class Handler[S <: Sys[S]](implicit cursor: stm.Cursor[S])
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
        cursor.step { implicit tx =>
          node.modelData().merge.name match {
            case Expr.Var(v) =>
              val expr = ExprImplicits[S]
              import expr._
              val s: Expr[S, String] = value.toString
              v() = s
          }
        }
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
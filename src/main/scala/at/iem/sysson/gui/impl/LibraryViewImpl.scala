package at.iem.sysson
package gui
package impl

import de.sciss.lucre.event.Sys
import de.sciss.treetable.TreeTableCellRenderer.State
import de.sciss.treetable.TreeColumnModel
import scala.swing.{Swing, ScrollPane, Component}
import de.sciss.model.Change
import de.sciss.treetable.j.DefaultTreeTableCellRenderer
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window
import java.awt.{Graphics, Color}

object LibraryViewImpl {
  def apply[S <: Sys[S]](library: Library[S])(implicit tx: S#Tx): LibraryView[S] = {
    val handler   = new Handler[S]
    val res       = new Impl(library, TreeTableView[S, Library[S], Handler[S]](library, handler))
    GUI.fromTx(res.guiInit())
    res
  }

  private final class Impl[S <: Sys[S]](val library: Library[S], treeView: TreeTableView[S, Library[S], Handler[S]])
    extends LibraryView[S] with ComponentHolder[Component] {
    impl =>

    def guiInit(): Unit = {
      val scroll = new ScrollPane(treeView.component)
      scroll.border = Swing.EmptyBorder
      component = scroll

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

  private final class Handler[S <: Sys[S]] extends TreeTableView.Handler[S, Library[S], Handler[S]] {
    type D  = String
    type BD = String
    type LD = String

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

    object columns extends TreeColumnModel[String] {
      def setValueAt(value: Any, node: String, column: Int): Unit = {
        ???
      }

      def getColumnName(column: Int) = "Name"

      def getColumnClass(column: Int) = classOf[String]

      def columnCount = 1

      def getValueAt(node: String, column: Int) = node

      def isCellEditable(node: String, column: Int) = false

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
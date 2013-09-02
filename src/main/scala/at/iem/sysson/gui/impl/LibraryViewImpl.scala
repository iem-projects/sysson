package at.iem.sysson
package gui
package impl

import scalaswingcontrib.tree.{Tree, ExternalTreeModel, TreeModel}
import scala.swing.{ScrollPane, Swing}
import scalaswingcontrib.event.TreeNodeSelected
import ucar.nc2
import javax.swing.tree.DefaultTreeCellRenderer
import javax.swing.{JComponent, TransferHandler, JTree}
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window
import java.awt.datatransfer.Transferable

object LibraryViewImpl {
  def apply(library: Library): LibraryView = {
    val tm = new ExternalTreeModel[Library.Node](library.root :: Nil, {
      case Library.Branch(_, children) => children
      case _ => Nil
    })

    val tree      = new Tree(tm) {
      selection.mode  = Tree.SelectionMode.Single
      renderer        = Tree.Renderer.wrap(LibraryRenderer)
      // listenTo(selection)
      // reactions += {
      //   case TreeNodeSelected(node: Library.Node) => // XXX TODO
      // }

      dragEnabled     = true
      peer.setTransferHandler(new TransferHandler {
        // ---- export ----

        override def getSourceActions(c: JComponent): Int =
          TransferHandler.LINK | TransferHandler.COPY | TransferHandler.MOVE // dragging only works when MOVE is included. Why?

        override def createTransferable(c: JComponent): Transferable = {
          val opt = selection.paths.collectFirst {
            case _ :+ Library.Child(patch) => patch
          }

          val res = opt.map { patch =>
            DragAndDrop.Transferable(PatchFlavor)(patch)
          } .orNull

          // println(s"createTransferable: $res")
          res
        }
      })

      expandAll()
    }
    val scroll    = new ScrollPane(tree)
    scroll.border = null

    new Impl(library, tree)
  }

  private object LibraryRenderer extends DefaultTreeCellRenderer {
    override def getTreeCellRendererComponent(tree: JTree, value: Object, isSelected: Boolean, isExpanded: Boolean,
                                              isLeaf: Boolean, row: Int, hasFocus: Boolean): java.awt.Component = {
      val v2 = value match {
        case n: Library.Node => n.name
        case _ => value
      }
      super.getTreeCellRendererComponent(tree, v2, isSelected, isExpanded, isLeaf, row, hasFocus)
    }
  }

  private final class Impl(val library: Library, val component: Tree[Library.Node]) extends LibraryView {
    impl =>

    private val f = new WindowImpl {
      frame =>

      def style   = Window.Regular
      def handler = SwingApplication.windowHandler

      title     = "Library"
      contents  = impl.component
      closeOperation = Window.CloseDispose
      pack()
      GUI.placeWindow(this, 1f, 0.25f, 20) // .centerOnScreen(this)
      front()
    }
  }
}

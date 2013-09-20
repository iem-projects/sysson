/*
 *  LibraryViewImpl.scala
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
package gui
package impl

import scalaswingcontrib.tree.{Tree, ExternalTreeModel}
import scala.swing.{BorderPanel, FlowPanel, Button, Component, ScrollPane}
import javax.swing.tree.DefaultTreeCellRenderer
import javax.swing.{JComponent, TransferHandler, JTree}
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.{OptionPane, Window}
import java.awt.datatransfer.Transferable
import scalaswingcontrib.event.TreeNodeSelected

object LibraryViewImpl {
  def apply(library: Library): LibraryView = {
    val tm = new ExternalTreeModel[Library.Node](library.root :: Nil, {
      case Library.Branch(_, children) => children
      case _ => Nil
    })

    lazy val tree = new Tree(tm) {
      selection.mode  = Tree.SelectionMode.Single
      renderer        = Tree.Renderer.wrap(LibraryRenderer)
      listenTo(selection)
      reactions += {
        case TreeNodeSelected(Library.Branch(_, _)) =>
          ggDelete.enabled  = true
          ggEdit  .enabled  = false

        case TreeNodeSelected(Library.Child(_)) =>
          ggDelete.enabled  = true
          ggEdit  .enabled  = true
      }

      dragEnabled     = true
      peer.setTransferHandler(new TransferHandler(null) {
        // ---- export ----

        override def getSourceActions(c: JComponent): Int =
          TransferHandler.LINK | TransferHandler.COPY | TransferHandler.MOVE // dragging only works when MOVE is included. Why?

        override def createTransferable(c: JComponent): Transferable = {
          val opt = selection.paths.collectFirst {
            case _ :+ Library.Child(patch) => patch
          }

          val res = opt.map { patch =>
            DragAndDrop.Transferable(PatchSourceFlavor)(patch)
          } .orNull

          // println(s"createTransferable: $res")
          res
        }
      })

      expandAll()
    }
    lazy val scroll = new ScrollPane(tree)
    scroll.border = null

    def selectedNode: Option[Library.Node] =
      tree.selection.paths.headOption.flatMap(_.lastOption)

    lazy val ggAdd = Button("+") {
      val opt   = OptionPane.textInput(message = "Patch Name:", initial = "Patch")
      opt.title = "Add New Patch"
      opt.show(Some(impl.frame)).foreach { name =>
        println(s"TODO: Add $name")
      }
    }
    ggAdd.peer.putClientProperty("JButton.buttonType", "roundRect")

    lazy val ggDelete: Button = Button("\u2212") {
      selectedNode.foreach {
        case Library.Branch(name, children) =>
          println(s"TODO: Delete")
        case Library.Child(source) =>
          println(s"TODO: Delete")
      }
      ggDelete.enabled = false
      ggEdit  .enabled = false
    }
    ggDelete.enabled = false
    ggDelete.peer.putClientProperty("JButton.buttonType", "roundRect")

    lazy val ggEdit: Button = Button("Edit") {
      selectedNode.foreach {
        case Library.Child(source) =>
          println(s"TODO: Edit ${source.name}")
        case _ =>
      }
    }
    ggEdit.enabled = false
    ggEdit.peer.putClientProperty("JButton.buttonType", "roundRect")

    lazy val butPanel = new FlowPanel(ggAdd, ggDelete, ggEdit)

    lazy val panel = new BorderPanel {
      add(tree    , BorderPanel.Position.Center)
      add(butPanel, BorderPanel.Position.South )
    }

    lazy val impl: Impl = new Impl(library, panel)
    impl
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

  private final class Impl(val library: Library, val component: Component) extends LibraryView {
    impl =>

    val frame = new WindowImpl {
      frame =>

      def style       = Window.Regular
      def handler     = SwingApplication.windowHandler

      title           = "Library"
      contents        = impl.component
      closeOperation  = Window.CloseDispose
      pack()
      GUI.placeWindow(this, 1f, 0.25f, 20)
      front()
    }
  }
}

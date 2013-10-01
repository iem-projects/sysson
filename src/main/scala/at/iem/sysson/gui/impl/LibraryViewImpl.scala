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
import scala.swing.{Action, BorderPanel, FlowPanel, Button, Component, ScrollPane}
import javax.swing.tree.DefaultTreeCellRenderer
import javax.swing.{JComponent, TransferHandler, JTree}
import de.sciss.desktop.impl.{UndoManagerImpl, WindowImpl}
import de.sciss.desktop.{UndoManager, OptionPane, Window}
import java.awt.datatransfer.Transferable
import scalaswingcontrib.event.TreeNodeSelected

object LibraryViewImpl {
  private def mkTreeModel(library: Library) = new ExternalTreeModel[Library.Node](library.root :: Nil, {
    case Library.Branch(_, children) => children
    case _ => Nil
  })

  def apply(library: Library): LibraryView = {
    lazy val tree = new Tree(mkTreeModel(library)) {
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

    def selectedPath: Option[Vec[Library.Node]] =
      tree.selection.paths.headOption

    lazy val ggAdd = Button("+") {
      val opt   = OptionPane.textInput(message = "Patch Name:", initial = "Patch")
      opt.title = "Add New Patch"
      opt.show(Some(impl.frame)).foreach { name =>
        val p       = Patch.Source(name = name, code = "// Patch Synth Graph Code")
        val b @ Library.Branch(_, _) = tree.model.roots.head
        val newLib  = new Library {
          val root = b.copy(children = b.children :+ Library.Child(p))
        }
        impl.library = newLib
      }
    }
    ggAdd.peer.putClientProperty("JButton.buttonType", "roundRect")

    lazy val ggDelete: Button = Button("\u2212") {
      selectedNode.foreach { node =>
        val opt = node match {
          case Library.Branch(name, children) =>
            OptionPane.confirmation(message = s"Delete branch '$name' with ${children.size} children?",
              optionType = OptionPane.Options.OkCancel, messageType = OptionPane.Message.Warning)
          case Library.Child(source) =>
            OptionPane.confirmation(message = s"Delete patch '${source.name}'?",
              optionType = OptionPane.Options.OkCancel, messageType = OptionPane.Message.Warning)
        }
        opt.title = "Delete Library Node"
        val res = opt.show(Some(impl.frame))
        if (res == OptionPane.Result.Ok) {
          println(s"TODO: Delete $node")
        }
      }
      ggDelete.enabled = false
      ggEdit  .enabled = false
    }
    ggDelete.enabled = false
    ggDelete.peer.putClientProperty("JButton.buttonType", "roundRect")

    lazy val ggEdit: Button = Button("Edit") {
      selectedPath.foreach {
        case oldPath @ parent :+ Library.Child(source) =>
          PatchCodeFrameImpl(source.name, Code.SynthGraph(source.code)) { (newName, newCode) =>
            val newChild: Library.Node = Library.Child(Patch.Source(name = newName, code = newCode))
            val indices = oldPath.sliding(2, 1).map { case Seq(p: Library.Branch, c) => p.children.indexOf(c) }
            val newRoot = (newChild /: (parent zip indices.toList)) {
              case (c, (p: Library.Branch, idx)) => p.copy(children = p.children.updated(idx, c))
            }
            val newLib = new Library {
              val root = newRoot.asInstanceOf[Library.Branch] // XXX TODO ugly
            }
            impl.library = newLib
          }
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

    lazy val impl: Impl = new Impl(library, tree, panel)
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

  private final class Impl(private var _library: Library, tree: Tree[Library.Node],
                           val component: Component)
    extends LibraryView {
    impl =>

    lazy val saveAction: Action = new Action(null) {
      enabled = false
      def apply(): Unit = ()
    }

    lazy val undoManager: UndoManager = new UndoManagerImpl {
      private var _dirty = false
      def dirty = _dirty
      def dirty_=(value: Boolean): Unit =
        if (_dirty != value) {  // crucial in init because we have two cyclic lazy vals (undoManager and frame)!
          _dirty = value
          frame.setDirtyFlag(value)
          saveAction.enabled = value
        }
    }

    object frame extends WindowImpl {
      frame =>

      def style       = Window.Regular
      def handler     = SwingApplication.windowHandler

      title           = "Library"
      contents        = impl.component
      closeOperation  = Window.CloseDispose

      bindMenus(
        "file.save" -> saveAction,
        "edit.undo" -> undoManager.undoAction,
        "edit.redo" -> undoManager.redoAction
      )

      pack()
      GUI.placeWindow(this, 1f, 0.25f, 20)

      def setDirtyFlag(value: Boolean): Unit = dirty = value
    }

    def library: Library = _library
    def library_=(value: Library): Unit = {
      _library    = value
      tree.model  = mkTreeModel(value)
      tree.expandAll()
    }

    // ---- constructor ----
    frame.front()
  }
}

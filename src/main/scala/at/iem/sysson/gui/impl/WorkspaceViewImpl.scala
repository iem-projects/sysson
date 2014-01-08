/*
 *  WorkspaceViewImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
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

import de.sciss.synth
import scala.swing.{TabbedPane, Label, FlowPanel, Component, BoxPanel, Orientation, Action, Swing}
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.{FileDialog, Window}
import de.sciss.file._
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.LinkedList
import de.sciss.lucre.synth.expr.ExprImplicits
import de.sciss.icons.raphael
import scala.util.control.NonFatal
import java.io.RandomAccessFile
import de.sciss.lucre.stm
import javax.swing.undo.{CannotRedoException, CannotUndoException, AbstractUndoableEdit}

object WorkspaceViewImpl {
  import Implicits._

  def apply[S <: Sys[S]](workspace: Workspace[S])(implicit tx: S#Tx): WorkspaceView[S] = {
    import workspace.cursor
    implicit val ws     = workspace
    implicit val llSer  = LinkedList.serializer[S, DataSource[S]]
    val dataSources = ListView[S, DataSource[S], Unit](workspace.dataSources)(_.file.name)
    val res = new Impl[S](dataSources)(workspace)
    GUI.fromTx(res.guiInit())
    res
  }

  // trait Disposable { def dispose(): Unit }

  private final val txtAddDataSource    = "Add Data Source"
  private final val txtRemoveDataSource = "Remove Data Source"
  private final val txtViewDataSource   = "View Data Source"

  private final class Impl[S <: Sys[S]](dataSources: ListView[S, DataSource[S], Unit])
                                       (implicit val workspace: Workspace[S])
    extends WorkspaceView[S] {

    impl =>

    var component: Component = _
    private var frame: WindowImpl = _

    import workspace.cursor

    // XXX TODO: DRY (LibraryViewImpl)
    // direction: true = insert, false = remove
    private class EditNode[A] protected(direction: Boolean,
                                     parentH: stm.Source[S#Tx, LinkedList.Modifiable[S, A, _]],
                                     index: Int,
                                     childH: stm.Source[S#Tx, A])
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

    def openSourceDialog(): Unit = {
      val dlg = FileDialog.open(title = txtAddDataSource)
      dlg.setFilter(util.NetCdfFileFilter)
      dlg.show(Some(frame)).foreach { f =>
        // DocumentHandler.instance.openRead(f.getPath)
        cursor.step { implicit tx =>
          val ds = DataSource[S](f.path)
          workspace.dataSources.addLast(ds)
        }
      }
    }

    def guiInit(): Unit = {
      GUI.requireEDT()

      val actionAddSource = new Action(null) {
        def apply(): Unit = openSourceDialog()
      }
      val ggAddSource = GUI.toolButton(actionAddSource, raphael.Shapes.Plus, tooltip = s"$txtAddDataSource...")

      val actionRemoveSource = new Action(null) {
        enabled = false

        def apply(): Unit = {
          println("TODO: Remove")
        }
      }
      val ggRemoveSource = GUI.toolButton(actionRemoveSource, raphael.Shapes.Minus, tooltip = txtRemoveDataSource)

      val actionViewSource = new Action(null) {
        enabled = false

        def apply(): Unit = {
          val sel = dataSources.guiSelection
          if (sel.nonEmpty) cursor.step { implicit tx =>
            sel.foreach { idx =>
              dataSources.list.foreach { ll =>
                val ds = ll(idx)
                DataSourceView(ds)
              }
            }
          }
        }
      }
      val ggViewSource = GUI.toolButton(actionViewSource, raphael.Shapes.View, tooltip = txtViewDataSource)

      val flow  = new FlowPanel(ggAddSource, ggRemoveSource, ggViewSource)

      val pageSources = new TabbedPane.Page("Data Sources", new BoxPanel(Orientation.Vertical) {
        contents += dataSources.component
        contents += flow
      })

      val ggTab = new TabbedPane {
        pages += pageSources
      }

      component = ggTab

      frame = new WindowImpl {
        def style   = Window.Regular
        def handler = SwingApplication.windowHandler

        title     = workspace.name
        file      = Some(workspace.dir)
        contents  = ggTab
        closeOperation = Window.CloseIgnore
        reactions += {
          case Window.Closing(_) =>
          // this will be recognized by the DocumentViewHandler which invokes dispose() on this view subsequently:
          //          document.close()
          case Window.Activated(_) =>
            DocumentViewHandler.instance.activeDocument = Some(workspace)
        }

        bindMenus(
          "file.close" -> Action(null) {
            //          document.close()
          }
        )

        pack()
        GUI.centerOnScreen(this)
        front()
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = GUI.fromTx(frame.dispose())
  }
}
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
import scala.swing.{FlowPanel, Component, BoxPanel, Orientation, Action, Swing}
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.{FileDialog, Window}
import de.sciss.file._
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.LinkedList
import de.sciss.lucre.synth.expr.ExprImplicits
import de.sciss.icons.raphael
import scala.util.control.NonFatal
import java.io.RandomAccessFile

object WorkspaceViewImpl {
  import Implicits._

  def apply[S <: Sys[S]](workspace: Workspace[S])(implicit tx: S#Tx): WorkspaceView[S] = {
    import workspace.{cursor, dataSourceSerializer}
    implicit val llSer = LinkedList.serializer[S, DataSource[S]]
    val dataSources = ListView[S, DataSource[S], Unit](workspace.dataSources)(_.file.name)
    val res = new Impl[S](workspace, dataSources)
    GUI.fromTx(res.guiInit())
    res
  }

  // trait Disposable { def dispose(): Unit }

  private final val txtAddDataSource    = "Add Data Source"
  private final val txtRemoveDataSource = "Remove Data Source"
  private final val txtViewDataSource   = "View Data Source"

  private final class Impl[S <: Sys[S]](val workspace: Workspace[S],
                                        dataSources: ListView[S, DataSource[S], Unit])
    extends WorkspaceView[S] {

    impl =>

    var component: Component = _
    private var frame: WindowImpl = _

    def openSourceDialog(): Unit = {
      val dlg = FileDialog.open(title = txtAddDataSource)
      dlg.setFilter(util.NetCdfFileFilter)
      dlg.show(Some(frame)).foreach { f =>
        // DocumentHandler.instance.openRead(f.getPath)
        println(s"TODO: Add $f")
      }
    }

    def guiInit(): Unit = {
      component = new BoxPanel(Orientation.Vertical) {
        contents ++= Seq(
          dataSources.component
        )
      }

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
          println("TODO: View")
        }
      }
      val ggViewSource = GUI.toolButton(actionViewSource, raphael.Shapes.View, tooltip = txtViewDataSource)

      val flow  = new FlowPanel(ggAddSource, ggRemoveSource, ggViewSource)

      frame = new WindowImpl {
        def style   = Window.Regular
        def handler = SwingApplication.windowHandler

        title     = workspace.name
        file      = Some(workspace.dir)
        contents  = new BoxPanel(Orientation.Vertical) {
          contents += impl.component
          contents += flow
        }
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
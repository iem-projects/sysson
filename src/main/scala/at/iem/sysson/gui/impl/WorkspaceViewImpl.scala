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
import scala.swing.{Component, BoxPanel, Orientation, Action, Swing}
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window
import de.sciss.file._
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.LinkedList

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

  trait Disposable { def dispose(): Unit }

  private final class Impl[S <: Sys[S]](val workspace: Workspace[S],
                                        dataSources: ListView[S, DataSource[S], Unit])
    extends WorkspaceView[S] {

    impl =>

    var component: Component = _
    private var f: WindowImpl = _

    def guiInit(): Unit = {
      component = new BoxPanel(Orientation.Vertical) {
        contents ++= Seq(
          dataSources.component
        )
      }

      f = new WindowImpl {
        def style   = Window.Regular
        def handler = SwingApplication.windowHandler

        title     = workspace.name
        file      = Some(workspace.dir)
        contents  = impl.component
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

    def dispose()(implicit tx: S#Tx): Unit = GUI.fromTx(f.dispose())
  }
}
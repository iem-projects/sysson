/*
 *  PlotViewImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui
package impl

import de.sciss.desktop.UndoManager
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm
import de.sciss.lucre.swing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.mellite.Workspace

import scala.swing.{Label, Orientation, BoxPanel, Component}

object PlotViewImpl {
  def apply[S <: Sys[S]](plot: Plot.Obj[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): PlotView[S] = {
    implicit val undoMgr = new UndoManagerImpl
    val res = new Impl(tx.newHandle(plot))
    deferTx(res.guiInit())
    res
  }

  private final class Impl[S <: Sys[S]](plotH: stm.Source[S#Tx, Plot.Obj[S]])
                                       (implicit val workspace: Workspace[S], val cursor: stm.Cursor[S],
                                        val undoManager: UndoManager)
    extends PlotView[S] with ComponentHolder[Component] {

    def plot(implicit tx: S#Tx): Plot.Obj[S] = plotH()

    def guiInit(): Unit = {
      component = new BoxPanel(Orientation.Vertical) {
        contents += new Label("TODO")
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = ()
  }
}

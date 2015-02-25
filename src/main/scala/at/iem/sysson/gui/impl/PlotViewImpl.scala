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

import java.awt.datatransfer.Transferable
import javax.swing.undo.UndoableEdit

import at.iem.sysson.gui.DragAndDrop.PlotMappingDrag
import de.sciss.desktop.UndoManager
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.lucre.event.Sys
import de.sciss.lucre.matrix.Matrix
import de.sciss.lucre.stm
import de.sciss.lucre.swing.edit.EditVar
import de.sciss.lucre.swing.{View, deferTx}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.mellite.Workspace
import de.sciss.serial.Serializer

import scala.swing.{Orientation, BoxPanel, Component}

object PlotViewImpl {
  def apply[S <: Sys[S]](plot: Plot.Obj[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): PlotView[S] = {
    implicit val undoMgr    = new UndoManagerImpl
    val isEditable          = Matrix.Var.unapply(plot.elem.peer.matrix).isDefined
    val plotMatrixView      = new PlotMatrixView(canSetMatrix = isEditable).init(plot)
    val chartView           = PlotChartImpl(plot)
    val statsView           = PlotStatsView(plot)
    val res                 = new Impl(chartView, plotMatrixView, statsView).init()
    res
  }

  private final class Impl[S <: Sys[S]](chartView: View[S], matrixView: PlotMatrixView[S], statsView: PlotStatsView[S])
    extends PlotView[S] with ComponentHolder[Component] {

    def workspace   : Workspace[S]  = matrixView.workspace
    def cursor      : stm.Cursor[S] = matrixView.cursor
    def undoManager : UndoManager   = matrixView.undoManager

    def plot(implicit tx: S#Tx): Plot.Obj[S] = matrixView.plotH()

    def init()(implicit tx: S#Tx): this.type = {
      deferTx(guiInit())
      this
    }

    private def guiInit(): Unit = {
      component = new BoxPanel(Orientation.Vertical) {
        contents += chartView .component
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += matrixView.component
          contents += statsView .component
        }
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      chartView .dispose()
      matrixView.dispose()
      statsView .dispose()
    }
  }

  private final class PlotAssocView[S <: Sys[S]](protected val sourceH: stm.Source[S#Tx, Plot[S]], keyName: String)
                                                (implicit  workspace: Workspace[S], undoManager: UndoManager,
                                                 cursor: stm.Cursor[S])
    extends DimAssocViewImpl[S](keyName) {

    type Source[S1 <: Sys[S1]] = Plot[S1]

    protected def flavor: DragAndDrop.Flavor[PlotMappingDrag] = DragAndDrop.PlotMappingFlavor
  }

  private final class PlotMatrixView[S <: Sys[S]](protected val canSetMatrix: Boolean)
                                                 (implicit val workspace: Workspace[S], val undoManager: UndoManager,
                                                  val cursor: stm.Cursor[S])
    extends MatrixAssocViewImpl[S](Vec(Plot.HKey, Plot.VKey)) { impl =>

    protected type Source[S1 <: Sys[S1]] = Plot[S1]

    protected def canRemoveMatrix: Boolean = false

    protected def editRemoveMatrix()(implicit tx: S#Tx): Option[UndoableEdit] = None

    protected def matrix(source: Source[S])(implicit tx: S#Tx): Matrix[S] = source.matrix

    protected def sourceSerializer: Serializer[S#Tx, S#Acc, Source[S]] = Plot.serializer[S]

    protected def mkAssocView(source: Source[S], key: String)(implicit tx: S#Tx): View[S] = {
      val dims      = source.dims
      val res       = new PlotAssocView[S](tx.newHandle(source), key)
      res.init(dims)
      res
    }

    private var _plotH: stm.Source[S#Tx, Plot.Obj[S]] = _

    def plotH: stm.Source[S#Tx, Plot.Obj[S]] = _plotH

    def init(plot: Plot.Obj[S])(implicit tx: S#Tx): this.type = {
      _plotH = tx.newHandle(plot)
      init()
      updateSource(Some(plot.elem.peer))
      this
    }

    protected def mkDimAssocTransferable(src: stm.Source[S#Tx, Source[S]], key0: String): Transferable =
      DragAndDrop.Transferable(DragAndDrop.PlotMappingFlavor)(new PlotMappingDrag {
        type S1 = S
        def source: stm.Source[S#Tx, Source[S]] = src
        def key: String = key0
        def workspace: Workspace[S] = impl.workspace
      })

    protected def editDropMatrix(m: Matrix[S])(implicit tx: S#Tx): Option[UndoableEdit] =
      Matrix.Var.unapply(plotH().elem.peer.matrix).map { vr =>
        EditVar("Set Matrix", vr, m)
      }
  }
}
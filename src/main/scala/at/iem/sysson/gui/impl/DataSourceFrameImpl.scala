/*
 *  DataSourceFrameImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2017 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.desktop.Menu
import de.sciss.lucre.matrix.DataSource
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.mellite.gui.impl.WindowImpl
import de.sciss.synth.proc.Workspace
import ucar.nc2

import scala.swing.Action

object DataSourceFrameImpl {
  def apply[S <: Sys[S]](source: DataSource[S])(implicit tx: S#Tx, workspace: Workspace[S],
                                                cursor: stm.Cursor[S]): DataSourceFrame[S] = {
    val view  = DataSourceView(source)
    val res   = new Impl(view)
    res.init()
    res
  }

  private final class Impl[S <: Sys[S]](val view: DataSourceView[S])
    extends WindowImpl[S] with DataSourceFrame[S] {

    import view.selectedVariable

    private[this] var actionPlot1D    : Action = _
    private[this] var actionPlotDist  : Action = _
    private[this] var actionAnomalies : Action = _
    private[this] var actionConcat    : Action = _
    private[this] var actionMap       : Action = _

    private def updateState(selected: Option[nc2.Variable]): Unit = {
      import Implicits._
      actionPlot1D    .enabled  = selected.exists(_.reducedRank == 1)
      actionPlotDist  .enabled  = selected.isDefined
      actionAnomalies .enabled  = selected.exists(_.dimensionMap.keySet.exists(_.toLowerCase == "time"))
      actionConcat    .enabled  = selected.isDefined
      actionMap       .enabled  = selected.isDefined
    }

    override protected def initGUI(): Unit = {
      val root  = window.handler.menuFactory
      val path  = "actions"
      root.get(path) match {
        case Some(g: Menu.Group) =>
          val sw = Some(window)
          actionPlot1D    = new ActionPlot1D            (sw, selectedVariable)
          actionPlotDist  = new ActionPlotDistribution  (sw, selectedVariable)
          actionAnomalies = new ActionCalculateAnomalies(sw, selectedVariable)
          actionConcat    = new ActionConcatMatrices    (sw, view)
          actionMap       = new ActionMapMatrixElements (sw, view)
          g.add(sw, Menu.Item("plot-1d"          , actionPlot1D   ))
          g.add(sw, Menu.Item("plot-distribution", actionPlotDist ))
          g.add(sw, Menu.Item("create-anomalies" , actionAnomalies))
          g.add(sw, Menu.Item("create-concat"    , actionConcat   ))
          g.add(sw, Menu.Item("create-map"       , actionMap      ))

        case _ => sys.error(s"No menu group for path '$path'")
      }

      updateState(view.selectedVariable)
      view.addListener {
        case DataSourceView.VariableSelection(v) => updateState(v)
      }
    }
  }
}
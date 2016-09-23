/*
 *  ActionPlot1D.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2016 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2016 Hanns Holger Rutz. All rights reserved.
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

import java.awt.Color

import de.sciss.desktop
import de.sciss.desktop.{DialogSource, OptionPane, Window, WindowHandler}
import de.sciss.lucre.swing.defer
import de.sciss.mellite.Application
import org.jfree.chart.ChartPanel
import ucar.nc2

import scala.concurrent.{Future, blocking}
import scala.swing.{Action, Component}
import scala.util.{Failure, Success}
import scalax.chart.Chart

final class ActionPlot1D(windowOpt: Option[Window], selectedVariable: => Option[nc2.Variable])
  extends Action("Plot 1D...") {

  def apply(): Unit =
    selectedVariable.foreach { vr =>
      import Implicits._
      if (vr.reducedRank != 1) {
        OptionPane.message(s"Variable ${vr.name} has ${vr.reducedRank} dimensions. Must have one.",
          OptionPane.Message.Error).show(windowOpt, title = "Plot 1D")
      } else {
        import Stats.executionContext
        val futData = Future {
          blocking {
            val dataV   = vr.readSafe().float1D
            val dimName = vr.reducedDimensions.head.name
            val vd      = vr.file.variableMap(dimName)
            val dataD   = vd.readSafe().float1D
            (dataV, dataD)
          }
        }
        futData.onComplete {
          case Success((dataV, dataD)) => defer {
            import scalax.chart.api._
            val data: Vec[(Float, Float)] = dataD zip dataV
            val dataCol = data.toXYSeriesCollection(title)
            val chart   = XYLineChart(dataCol, title = title, legend = false)
            ChartUtils.printableLook(chart)
            val plot    = chart.plot
            val renderer = plot.getRenderer
            renderer.setSeriesPaint(0, Color.darkGray)
            mkPlotWindow(mkChartPanel(chart), vr.name)
          }
          case Failure(ex) => defer {
            DialogSource.Exception(ex -> s"Plot of ${vr.name}").show(windowOpt)
          }
        }
      }
    }

  private def mkChartPanel(chart: Chart): Component = Component.wrap(new ChartPanel(chart.peer))

  private def mkPlotWindow(chartPanel: Component, title0: String): Window =
    new desktop.impl.WindowImpl { win =>
      def handler: WindowHandler = Application.windowHandler
      contents = chartPanel
      title = title0
      pack()
      desktop.Util.centerOnScreen(this)
      front()
    }
}
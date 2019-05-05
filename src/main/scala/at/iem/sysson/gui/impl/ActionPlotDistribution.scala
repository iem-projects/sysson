/*
 *  ActionPlotDistribution.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2019 Hanns Holger Rutz. All rights reserved.
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

import java.awt.{Color, Graphics}

import de.sciss.chart.{Chart, XYChart}
import de.sciss.desktop.{DialogSource, Window, WindowHandler}
import de.sciss.lucre.swing.defer
import de.sciss.mellite.Application
import de.sciss.{desktop, kollflitz, numbers}
import org.jfree.chart.ChartPanel
import org.jfree.chart.renderer.xy.XYStepAreaRenderer
import ucar.nc2

import scala.collection.breakOut
import scala.concurrent.stm.atomic
import scala.swing.{Action, Component, Orientation, SplitPane}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

final class ActionPlotDistribution(windowOpt: Option[Window], selectedVariable: => Option[nc2.Variable])
  extends Action("Plot Distribution...") {

  import Implicits._

  def apply(): Unit =
    selectedVariable.foreach { vr =>
      val title0 = s"Distribution of ${vr.name}"

      import at.iem.sysson.Stats.executionContext
      val futStats = atomic { implicit tx =>
        Stats.get(vr.file)
      }
      val futHistoStats = futStats.map { case Stats(map) =>
        val total   = map(vr.name).total
        val numBins = 400 // XXX TODO -- could be configurable
      // val bins    = (0 until numBins).linlin(0, numBins - 1, total.min, total.max)
        val histo   = new Array[Int](numBins)

        def loop(rem: Vec[nc2.Dimension], red: VariableSection): Unit = rem match {
          case head +: (tail @ _ +: _) if red.size > 16384 =>   // try to make smart chunks
            for (i <- 0 until head.size) loop(tail, red.in(head.name).select(i))
          case _ =>
            red.variable.fillValue
            val chunk = red.readSafe().double1D.dropNaNs(red.variable.fillValue)
            chunk.foreach { d =>
              import numbers.Implicits._
              val bin = d.linLin(total.min, total.max, 0, numBins).toInt.min(numBins - 1)
              histo(bin) += 1
            }
        }
        loop(vr.reducedDimensions, vr.selectAll)
        (histo, total)
      }
      println("Calculating... Please wait...")

      futHistoStats.onComplete {
        case Success((histo, total)) => defer {
          val size      = (0L /: histo)((sum, count) => sum + count)
          val relative: Vec[Double] = histo.map(count => (count * 100.0)/ size)(breakOut)
          import kollflitz.Ops._
          val acc     = relative.integrate

          //            println("----- HISTO -----")
          //            histo.zipWithIndex.foreach { case (d, i) =>
          //                println(f"$i%3d: $d%1.4f")
          //            }

          val chartRel  = mkHistoChart(relative , total, title = s"Histogram for ${vr.name}")
          val chartAcc  = mkHistoChart(acc      , total, title = s"Accumulative Histogram for ${vr.name}")
          mkPlotWindow(new SplitPane(Orientation.Vertical, mkChartPanel(chartRel  ), mkChartPanel(chartAcc)),
            title0)
        }

        case Failure(ex) => defer {
          DialogSource.Exception(ex -> s"Distribution of ${vr.name}").show(windowOpt)
        }
      }
    }

  private def mkHistoChart(histo: Vec[Double], stats: Stats.Counts, title: String): XYChart = {
    import de.sciss.chart.module.Charting._
    import numbers.Implicits._
    val data: Vec[(Double, Double)] = histo.zipWithIndex.map { case (num, i) =>
      (i + 0.5).linLin(0, histo.length, stats.min, stats.max) -> num
    } (breakOut)
    val dataCol = data.toXYSeriesCollection(title)
    // val chart   = XYBarChart(dataCol, title = title, legend = false)
    val chart   = XYLineChart(dataCol)
    chart.title = title
    chart.subtitles.clear() // remove legend
    ChartUtils.printableLook(chart)
    val plot    = chart.plot
    // val rangeX  = plot.getDomainAxis.asInstanceOf[NumberAxis]
    // val renderer = plot.getRenderer // .asInstanceOf[XYBarRenderer]
    val renderer = new XYStepAreaRenderer()
    plot.setRenderer(renderer)
    renderer.setSeriesPaint(0, Color.darkGray)
    // rangeX.setTickUnit(new NumberTickUnit(1))
    // val rangeY  = plot.getRangeAxis.asInstanceOf[NumberAxis]
    // rangeY.setTickUnit(new NumberTickUnit(1))
    // plot.getRenderer.asInstanceOf[BarRenderer].setMaximumBarWidth(0.1)
    // renderer.setMargin(1.0 - 1.0/histo.length) // (0.0)
    // val bp = new StandardXYBarPainter
    // renderer.setBarPainter(bp)
    chart
  }

  private def mkChartPanel(chart: Chart): Component = {
    val jp = new ChartPanel(chart.peer) {
      override def paintComponent(g: Graphics): Unit =
        try {
          super.paintComponent(g)
        } catch {
          case NonFatal(ex) =>
            g.clearRect(0, 0, getWidth, getHeight)
            val fm = g.getFontMetrics
            g.setColor(Color.red)
            g.drawString(s"Error: ${ex.getMessage}", 4, 4 + fm.getAscent)
        }
    }
    Component.wrap(jp)
  }

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


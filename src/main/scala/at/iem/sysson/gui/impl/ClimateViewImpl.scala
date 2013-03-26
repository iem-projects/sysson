package at.iem.sysson
package gui
package impl

import ucar.nc2
import org.jfree.chart.{JFreeChart, ChartPanel}
import Implicits._
import org.jfree.chart.renderer.xy.XYBlockRenderer
import org.jfree.chart.renderer.GrayPaintScale
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.data.xy.{MatrixSeriesCollection, MatrixSeries}
import scala.Some
import java.awt.Color
import swing.{BorderPanel, Component, Alignment, Slider, Label, Swing}
import Swing._
import scalaswingcontrib.group.GroupPanel
import collection.immutable.{IndexedSeq => IIdxSeq}
import javax.swing.GroupLayout
import swing.event.ValueChanged
import language.reflectiveCalls

object ClimateViewImpl {
  private case class Reduction(name: Label, slider: Slider, value: Label)

  def apply(in: nc2.NetcdfFile, section: VariableSection): ClimateView = {
    def valueFun(dim: nc2.Dimension, units: Boolean): Int => String =
      in.variableMap.get(dim.name.getOrElse("?")) match {
        case Some(v) if v.isFloat =>
          val dat = v.read().float1D
          (i: Int) => f"${dat(i).toInt}%d${if (units) v.units.map(s => " " + s).getOrElse("") else ""}"

        case Some(v) if v.isDouble  =>
          val dat = v.read().double1D
          (i: Int) => f"${dat(i).toInt}%d${if (units) v.units.map(s => " " + s).getOrElse("") else ""}"

        case _ => (i: Int) => i.toString
      }

    val lat     = section.dimensions.find { d =>
      d.name.flatMap(in.variableMap.get _).flatMap(_.units) == Some("degrees_north")
    } getOrElse sys.error("Did not find latitude dimension")
    val lon     = section.dimensions.find { d =>
      d.name.flatMap(in.variableMap.get _).flatMap(_.units) == Some("degrees_east")
    } getOrElse sys.error("Did not find longitude dimension")

    val red     = section.reducedDimensions.filterNot(d => d == lat || d == lon)
    val width   = lon.size
    val height  = lat.size

    val data   = new MatrixSeries("Continguency", height, width)

    def updateData() {
      val sec = red.zipWithIndex.foldLeft(section) { case (res, (d, idx)) =>
        res in d.name.getOrElse("?") select redGUI(idx).slider.value
      }
      val arr = sec.read().float1D.normalize(sec.variable.fillValue)
      for (x <- 0 until width) {
        for (y <- 0 until height) {
          val z = arr(y * width + x)
          // data.setZValue(x, y, z)
          data.update(y, x, z)
        }
      }
    }

    lazy val redGUI: IIdxSeq[Reduction] = red.map { d =>
      val n   = d.name.getOrElse("?")
      val lb  = new Label(n + ":") {
        horizontalAlignment = Alignment.Trailing
        peer.putClientProperty("JComponent.sizeVariant", "small")
      }
      val m = valueFun(d, units = true)
      val curr = new Label {
        peer.putClientProperty("JComponent.sizeVariant", "small")
        text    = m(d.size - 1)
        val szm = preferredSize
        text    = m(0)
        val sz0 = preferredSize
        preferredSize = (math.max(sz0.width, szm.width), math.max(sz0.height, szm.height))
      }
      val sl  = new Slider {
        min   = 0
        max   = d.size - 1
        value = 0
        peer.putClientProperty("JComponent.sizeVariant", "small")
        listenTo(this)
        reactions += {
          case ValueChanged(_) =>
            curr.text = m(value)
            if (!adjusting) updateData()
        }
      }

      Reduction(lb, sl, curr)
    }

    val redGroup  = new GroupPanel {
      theHorizontalLayout is Sequential(
        Parallel(redGUI.map(r => add[GroupLayout#ParallelGroup](r.name  )): _*),
        Parallel(redGUI.map(r => add[GroupLayout#ParallelGroup](r.slider)): _*),
        Parallel(redGUI.map(r => add[GroupLayout#ParallelGroup](r.value )): _*)
      )

      theVerticalLayout is Sequential(
        (redGUI.map { r =>
          Parallel(Center)(r.name, r.slider, r.value): InGroup[GroupLayout#SequentialGroup]
        }): _*
      )
    }

    updateData()

    val renderer  = new XYBlockRenderer()
    val scale     = new GrayPaintScale(0.0, 1.0)
    renderer.setPaintScale(scale)
    val xAxis     = new NumberAxis("Longitude")
    xAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())
    xAxis.setLowerMargin(0.0)
    xAxis.setUpperMargin(0.0)
    val yAxis     = new NumberAxis("Latitude")
    yAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())
    yAxis.setLowerMargin(0.0)
    yAxis.setUpperMargin(0.0)
    val coll      = new MatrixSeriesCollection(data)
    val plot      = new XYPlot(coll, xAxis, yAxis, renderer)

    plot.setBackgroundPaint(Color.lightGray)
    plot.setDomainGridlinesVisible(false)
    plot.setRangeGridlinePaint(Color.white)
    val chart     = new JFreeChart(section.variable.name, plot)
    chart.removeLegend()
    chart.setBackgroundPaint(Color.white)
    val view    = new Impl(chart, redGroup)
    view
  }

  private final class Impl(chart: JFreeChart, redGroup: Component) extends ClimateView {
    private val main  = new ChartPanel(chart)
    val component = new BorderPanel {
      add(Component.wrap(main), BorderPanel.Position.Center)
      add(redGroup, BorderPanel.Position.South)
    }
  }
}
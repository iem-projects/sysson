package at.iem.sysson
package gui
package impl

import javax.swing.JComponent
import java.awt.{Color, Graphics}
import swing.Component
import ucar.nc2
import org.jfree.chart.{JFreeChart, ChartPanel}
import org.jfree.data.general.DefaultHeatMapDataset
import Implicits._
import org.jfree.chart.renderer.xy.XYBlockRenderer
import org.jfree.chart.renderer.GrayPaintScale
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.data.xy.{MatrixSeriesCollection, MatrixSeries}

object ClimateViewImpl {
  def apply(in: nc2.NetcdfFile, section: VariableSection): ClimateView = {
    val lat     = section.dimensions.find { d =>
      d.name.flatMap(in.variableMap.get _).flatMap(_.units) == Some("degrees_north")
    } getOrElse sys.error("Did not find latitude dimension")
    val lon     = section.dimensions.find { d =>
      d.name.flatMap(in.variableMap.get _).flatMap(_.units) == Some("degrees_east")
    } getOrElse sys.error("Did not find longitude dimension")

    val red     = section.reducedDimensions.filterNot(d => d == lat || d == lon)
    val sec     = red.foldLeft(section) { case (res, d) => res in d.name.getOrElse("???") select 0 }
    val width   = lon.size
    val height  = lat.size
    val arr     = sec.read().float1D.normalize

//    val data    = new DefaultHeatMapDataset(width, height, 0f, 1f, 0f, 1f)
    val data   = new MatrixSeries("Continguency", width, height)
    for (x <- 0 until width) {
      for (y <- 0 until height) {
        val z = arr(x * height + y)
        // data.setZValue(x, y, z)
        data.update(x, y, z)
      }
    }
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
    val chart     = new JFreeChart(sec.variable.name, plot)
    chart.removeLegend()
    chart.setBackgroundPaint(Color.white)
    val view    = new Impl(chart)
    view
  }

  private final class Impl(chart: JFreeChart) extends ClimateView {
    private val main  = new ChartPanel(chart)
    val component = Component.wrap(main)
  }
}
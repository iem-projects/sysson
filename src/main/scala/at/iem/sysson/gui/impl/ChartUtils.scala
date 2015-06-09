package at.iem.sysson.gui.impl

import java.awt.{Font, Color}

import org.jfree.chart.plot.{CategoryPlot, XYPlot}
import org.jfree.chart.renderer.xy.{StandardXYBarPainter, XYBarRenderer}

import scalax.chart.Chart

object ChartUtils {
  private lazy val defaultFontFace  = "Liberation Sans" // "Helvetica"  // "Arial"

  def printableLook(chart: Chart): Unit = {
    val plot      = chart.plot
    // val titleText = chart.title
    // chart.peer.setTitle(new TextTitle(titleText, new Font(titleFontFace, Font.BOLD, 22)))

    val (xAxis, yAxis) = plot match {  // shitty Plot / Renderer interfaces do not have common super types
      case p: XYPlot       =>
        p.setBackgroundPaint           (Color.white    )
        p.setDomainGridlinePaint       (Color.lightGray)
        p.setRangeGridlinePaint        (Color.lightGray)
        p.getRenderer.setSeriesPaint(0, Color.darkGray )
        // undo the crappy "3D" look
        p.getRenderer match {
          case r: XYBarRenderer => r.setBarPainter(new StandardXYBarPainter())
          case _ =>
        }
        (p.getDomainAxis, p.getRangeAxis)
      case p: CategoryPlot =>
        p.setBackgroundPaint           (Color.white    )
        p.setDomainGridlinePaint       (Color.lightGray)
        p.setRangeGridlinePaint        (Color.lightGray)
        p.getRenderer.setSeriesPaint(0, Color.darkGray )
        // undo the crappy "3D" look
        p.getRenderer match {
          case r: XYBarRenderer => r.setBarPainter(new StandardXYBarPainter())
          case _ =>
        }
        (p.getDomainAxis, p.getRangeAxis)
    }

    //      val xAxis         = plot.getDomainAxis
    //      val yAxis         = plot.getRangeAxis
    val fnt1          = new Font(defaultFontFace, Font.BOLD , 14)
    val fnt2          = new Font(defaultFontFace, Font.PLAIN, 12)
    xAxis.setLabelFont(fnt1)
    xAxis.setTickLabelFont(fnt2)
    yAxis.setLabelFont(fnt1)
    yAxis.setTickLabelFont(fnt2)
    // chart.peer.getTitle.setFont(new Font(titleFontFace, Font.BOLD, 22))
  }
}

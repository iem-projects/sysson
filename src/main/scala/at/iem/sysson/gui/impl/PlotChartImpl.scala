/*
 *  PlotChartImpl.scala
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

import java.awt.{Rectangle, TexturePaint, Color}
import java.awt.image.BufferedImage

import de.sciss.intensitypalette.IntensityPalette
import de.sciss.lucre.event.Sys
import de.sciss.lucre.matrix.{Matrix, DataSource}
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.swing.{defer, deferTx}
import de.sciss.lucre.swing.View
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.mellite.Workspace
import de.sciss.processor.impl.ProcessorImpl
import org.jfree.chart.axis.{SymbolAxis, NumberAxis}
import org.jfree.chart.renderer.{LookupPaintScale, PaintScale}
import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYBlockRenderer
import org.jfree.data.xy.{MatrixSeries, MatrixSeriesCollection}

import Implicits._

import scala.concurrent.ExecutionContext
import scala.concurrent.stm.Ref
import scala.swing.Component

object PlotChartImpl {
  def apply[S <: Sys[S]](plot: Plot.Obj[S])(implicit tx: S#Tx, workspace: Workspace[S]): View[S] = {
    implicit val resolver = WorkspaceResolver[S]
    new Impl[S].init(plot)
  }

  private final val DEBUG = true

  private final class PlotData(val hName: String, val hData: Array[Float],
                               val vName: String, val vData: Array[Float],
                               val mName: String, val mData: Array[Array[Float]])

  private final class Reader(mName: String, mReader: Matrix.Reader,
                             hName: String, hReader: Matrix.Reader,
                             vName: String, vReader: Matrix.Reader)
    extends ProcessorImpl[PlotData, Reader] {

    private def readDim(r: Matrix.Reader): Array[Float] = {
      assert(r.numChannels == 1)
      val sz    = r.numFrames.toInt
      val res   = new Array[Float](sz)
      r.read(Array(res), 0, sz)
      checkAborted()
      res
    }

    protected def body(): PlotData = {
      val hData = readDim(hReader)
      val vData = readDim(vReader)

      val mCh   = mReader.numChannels
      val mSz   = mReader.numFrames.toInt
      val mData = Array.ofDim[Float](mCh, mSz)
      mReader.read(mData, 0, mSz)
      checkAborted()

      new PlotData(hName = hName, hData = hData, vName = vName, vData = vData, mName = mName, mData = mData)
    }
  }

  private final class Impl[S <: Sys[S]](implicit resolver: DataSource.Resolver[S])
    extends View[S] with ComponentHolder[Component] {

    // checks if the shape is reducible in all but the provided dimensions
    private def checkShape1D(shape: Vec[Int], hIdx: Int, vIdx: Int): Boolean =
      shape.zipWithIndex.forall { case (n, i) => n == 1 || i == hIdx || i == vIdx }

    private val readerRef = Ref(new Reader(null, null, null, null, null, null))

    private def updateData(plot: Plot[S])(implicit tx: S#Tx): Unit = {
      val m       = plot.matrix
      val dimMap  = plot.dims
      val dims    = m.dimensions
      val hName   = dimMap.get(Plot.HKey).map(_.value).getOrElse("?")
      val vName   = dimMap.get(Plot.VKey).map(_.value).getOrElse("?")
      val hIdx    = dims.indexWhere(_.name == hName)
      val vIdx    = dims.indexWhere(_.name == vName)
      val mShape  = m.shape
      val shapeOk = checkShape1D(mShape, hIdx, vIdx)

      if (DEBUG) println(s"updateData. hIdx = $hIdx, vIdx = $vIdx, ok? $shapeOk")

      if (hIdx >= 0 && vIdx >= 0 && shapeOk) {
        val hKey    = m.getDimensionKey(hIdx, useChannels = false)
        val vKey    = m.getDimensionKey(vIdx, useChannels = false)
        val hReader = hKey.reader[S]()
        val vReader = vKey.reader[S]()
        val mName   = m.name  // or plot-obj attr name?
        val mReader = m.reader(streamDim = hIdx)  // rows = channels, columns = frames
        val proc    = new Reader(mName = mName, mReader = mReader, hName = hName, hReader = hReader, vName = vName, vReader = vReader)
        val oldProc = readerRef.swap(proc)(tx.peer)
        tx.afterCommit {
          oldProc.abort()
          import ExecutionContext.Implicits.global
          if (!proc.aborted) {
            proc.start()
            proc.foreach { plotData =>
              defer {
                if (DEBUG) println(s"X-Axis: ${plotData.hName}; ${plotData.hData.take(144).mkString(",")}")
                if (DEBUG) println(s"Y-Axis: ${plotData.vName}; ${plotData.vData.take(144).mkString(",")}")
                updatePlot(plotData)
              }
            }
            proc.onFailure {
              case ex =>
                Console.err.println("Matrix reader failed:")
                ex.printStackTrace()
            }
          }
        }
      }
    }

    private val dataset = new MatrixSeriesCollection

    // returns (add, mul)
    private def normalize(d: Array[Array[Float]], fill: Float): Array[Array[Float]] = {
      var min  = Float.MaxValue
      var max  = Float.MinValue

      val fillIsNaN = fill.isNaN
      var i = 0
      while (i < d.length) {
        val d1 = d(i)
        var j = 0
        while (j < d1.length) {
          val f = d1(j)
          if (java.lang.Float.isInfinite(f)) sys.error("Unbounded value: " + f)

          val isNaN = if (fillIsNaN) f.isNaN else f == fill
          if (!isNaN) {
            if (f < min) min = f
            if (f > max) max = f
          }
          j += 1
        }
        i += 1
      }
      val div = max - min
      if (div <= 0f) return d
      val mul = 1f / div
      val res = d.clone()

      i = 0
      while (i < d.length) {
        val d1 = d(i)
        var j = 0
        while (j < d1.length) {
          val f = d1(j)
          val isNaN = if (fillIsNaN) f.isNaN else f == fill
          if (!isNaN) d1(j) = (f - min) * mul
          j += 1
        }
        i += 1
      }
      res
    }

    // called on EDT
    private def updatePlot(data: PlotData): Unit = {
      val xAxis = createAxis(data.hName, data.hData)
      val yAxis = createAxis(data.vName, data.vData)
      dataset.removeAllSeries()
      _plot.setDomainAxis(xAxis)
      _plot.setRangeAxis (yAxis)

      val mData = normalize(data.mData, Float.NaN)  // XXX TODO -- find fill value
      val mRows = mData.length
      val mCols = if (mRows == 0) 0 else mData(0).length
      val ms = new MatrixSeries(data.mName, mRows, mCols) {
        private var i = 0
        while (i < mRows) {
          val in  = mData(i)
          val out = data(i)
          var j = 0
          while (j < mCols) {
            out(j) = in(j)  // double-to-float
            j += 1
          }
          i += 1
        }
      }
      dataset.addSeries(ms)
      // _plot.setDataset(dataset)

    }

    private var observer: Disposable[S#Tx] = _

    def init(plotObj: Plot.Obj[S])(implicit tx: S#Tx): this.type = {
      deferTx(guiInit())
      val plot = plotObj.elem.peer
      updateData(plot)
      observer = plot.changed.react { implicit tx => u =>
        if (DEBUG) println("plot changed")
        updateData(u.plot)
      }
      this
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      observer.dispose()
      val p = readerRef.get(tx.peer)
      deferTx(p.abort())
    }

    private lazy val intensityScale: PaintScale = {
      val nanImg = new BufferedImage(2, 2, BufferedImage.TYPE_INT_ARGB)
      nanImg.setRGB(0, 0, 0xFF000000)
      nanImg.setRGB(0, 1, 0xFFFFFFFF)
      nanImg.setRGB(1, 1, 0xFF000000)
      nanImg.setRGB(1, 1, 0xFFFFFFFF)
      val nanPaint = new TexturePaint(nanImg, new Rectangle(0, 0, 2, 2))

      val res = new LookupPaintScale(0.0, 1.0, nanPaint /* Color.red */)
      val numM = IntensityPalette.numColors - 1
      for(i <- 0 to numM) {
        val d   = i.toDouble / numM
        val pnt = new Color(IntensityPalette.apply(d.toFloat))
        res.add(d, pnt)
      }
      res
    }

    private def createAxis(name: String, data: Array[Float]): NumberAxis = {
      val nameC   = name.capitalize
      val labels  = data.map(_.toString)
      val res     = new SymbolAxis(nameC, labels)
      res.setStandardTickUnits(NumberAxis.createIntegerTickUnits())
      res.setLowerMargin(0.0)
      res.setUpperMargin(0.0)
      res
    }

    private var _plot: XYPlot = _

    private def guiInit(): Unit = {
      val renderer  = new XYBlockRenderer()
      renderer.setPaintScale(intensityScale)

      // val xAxis     = createAxis(xDim)
      // val yAxis     = createAxis(yDim)
      // val coll      = new MatrixSeriesCollection // (data)
      _plot = new XYPlot // (coll, xAxis, yAxis, renderer)
      _plot.setDataset(dataset)
      _plot.setRenderer(renderer)

      //    val xmValue = mkValueMarker(xDimRed)
      //    val ymValue = mkValueMarker(yDimRed)
      //    val xmRange = mkRangeMarker(xDimRed)
      //    val ymRange = mkRangeMarker(yDimRed)
      //
      //    plot.addDomainMarker(xmValue)
      //    plot.addRangeMarker (ymValue)
      //    plot.addDomainMarker(xmRange)
      //    plot.addRangeMarker (ymRange)

      _plot.setBackgroundPaint(Color.lightGray)
      _plot.setDomainGridlinesVisible(false)
      _plot.setRangeGridlinePaint(Color.white)
      val chart = new JFreeChart(/* section.variable.description.getOrElse(section.variable.name), */ _plot)

      chart.removeLegend()
      chart.setBackgroundPaint(Color.white)

      val main = new ChartPanel(chart, false)  // XXX TODO: useBuffer = false only during PDF export
      component = Component.wrap(main)
    }
  }
}
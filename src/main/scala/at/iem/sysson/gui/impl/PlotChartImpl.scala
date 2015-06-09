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

import java.awt.image.BufferedImage
import java.awt.{Paint, Color, Rectangle, Shape, TexturePaint}

import de.sciss.desktop.UndoManager
import de.sciss.icons.raphael
import de.sciss.intensitypalette.IntensityPalette
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.{String => StringEx, Boolean => BooleanEx}
import de.sciss.lucre.matrix.{DataSource, Matrix}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{CellView, BooleanCheckBoxView, View, defer, deferTx}
import de.sciss.mellite.Workspace
import de.sciss.mellite.gui.AttrCellView
import de.sciss.mellite.gui.edit.EditAttrMap
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.swingplus.{ComboBox, OverlayPanel}
import de.sciss.synth.proc.{StringElem, BooleanElem}
import org.jfree.chart.axis.{NumberAxis, SymbolAxis}
import org.jfree.chart.panel.{AbstractOverlay, Overlay}
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYBlockRenderer
import org.jfree.chart.renderer.{LookupPaintScale, PaintScale}
import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.data.xy.{MatrixSeries, MatrixSeriesCollection}

import scala.concurrent.Future
import scala.concurrent.stm.Ref
import scala.swing.{Alignment, Label, FlowPanel, Swing, BorderPanel, Component, Graphics2D}

object PlotChartImpl {
  def apply[S <: Sys[S]](plot: Plot.Obj[S])(implicit tx: S#Tx, cursor: stm.Cursor[S], undoManager: UndoManager,
                                            workspace: Workspace[S]): View.Editable[S] = {
    implicit val resolver = WorkspaceResolver[S]
    new Impl[S].init(plot)
  }

  private final val DEBUG = false

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

  private final class Impl[S <: Sys[S]](implicit val cursor: stm.Cursor[S], val undoManager: UndoManager,
                                        resolver: DataSource.Resolver[S])
    extends View.Editable[S] with ComponentHolder[Component] {

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
          import scala.concurrent.ExecutionContext.Implicits.global
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
              case Processor.Aborted() =>
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

    private var _lastXName: String       = _
    private var _lastYName: String       = _
    private var _lastXVals: Array[Float] = new Array[Float](0)  // we use `sameElements`, so ensure it's not null
    private var _lastYVals: Array[Float] = _lastXVals           // dito

    // called on EDT
    private def updatePlot(data: PlotData): Unit = {
      dataset.removeAllSeries()
      val xName     = data.hName
      val xVals     = data.hData
      val xChanged  = xName != _lastXName || !(xVals sameElements _lastXVals)
      if (xChanged) {
        _lastXName  = xName
        _lastXVals  = xVals
        val xAxis   = createAxis(xName, xVals)
        _plot.setDomainAxis(xAxis)
      }
      val yName     = data.vName
      val yVals     = data.vData
      val yChanged  = yName != _lastYName || !(yVals sameElements _lastYVals)
      if (yChanged) {
        _lastYName  = yName
        _lastYVals  = yVals
        val yAxis   = createAxis(yName, yVals)
        _plot.setRangeAxis(yAxis)
      }

      // XXX TODO -- look for stats
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

      if (xChanged || yChanged) {
        val xNameL    = xName.toLowerCase
        val yNameL    = yName.toLowerCase
        isMap         = (xNameL == "lon" || xName == "Longitude") && (yNameL == "lat" || yNameL == "Latitude")
        updateOverlay()
      }
    }

    private var isMap = false
    private var observerPlot    : Disposable[S#Tx] = _
    private var observerOverlay : Disposable[S#Tx] = _
    private var observerPalette : Disposable[S#Tx] = _

    private var viewOverlay     : BooleanCheckBoxView[S] = _
    private var overlayEmpty    : Component = _
    private var cellPalette     : CellView.Var[S, Option[String]] = _

    private var plotH: stm.Source[S#Tx, Plot.Obj[S]] = _

    // called on the EDT
    private def updateOverlay(): Unit = {
      overlayEmpty.visible  = isMap
      mapOverlay.enabled    = isMap && viewOverlay.component.selected
    }

    def init(plotObj: Plot.Obj[S])(implicit tx: S#Tx): this.type = {
      plotH = tx.newHandle(plotObj)

      implicit val booleanEx  = BooleanEx
      implicit val stringEx   = StringEx

      val cellOverlay = AttrCellView[S, Boolean, BooleanElem](plotObj, Plot.attrShowOverlay)
      viewOverlay     = BooleanCheckBoxView.optional(cellOverlay, name = "Map Overlay", default = false)
      observerOverlay = cellOverlay.react { implicit tx => _ => deferTx(updateOverlay()) }
      cellPalette     = AttrCellView[S, String, StringElem](plotObj, Plot.attrPalette)
      observerPalette = cellPalette.react { implicit tx => nameOpt =>
        deferTx {
          setPalette(nameOpt)
        }
      }

      val paletteValue0 = cellPalette()
      deferTx(guiInit(paletteValue0 = paletteValue0))
      val plot = plotObj.elem.peer
      updateData(plot)

      observerPlot = plot.changed.react { implicit tx => u =>
        if (DEBUG) println("plot changed")
        updateData(u.plot)
      }
      this
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      observerPlot    .dispose()
      observerOverlay .dispose()
      observerPalette .dispose()
      val p = readerRef.get(tx.peer)
      deferTx(p.abort())
    }

    private lazy val nanPaint: Paint = {
      val nanImg = new BufferedImage(2, 2, BufferedImage.TYPE_INT_ARGB)
      nanImg.setRGB(0, 0, 0xFF000000)
      nanImg.setRGB(0, 1, 0xFFFFFFFF)
      nanImg.setRGB(1, 1, 0xFF000000)
      nanImg.setRGB(1, 1, 0xFFFFFFFF)
      new TexturePaint(nanImg, new Rectangle(0, 0, 2, 2))
    }

    private def mkScale(name: String): PaintScale = new PaintScale {
      def getLowerBound: Double = 0.0
      def getUpperBound: Double = 1.0

      private val cpt   = ColorPaletteTable.builtIn(name)
      private val fill  = nanPaint
      private val pre   = if (cpt.isDiscrete) Array.tabulate(cpt.num)(s => new Color(cpt(s).lowColor)) else null
      private val bg    = new Color(cpt.background)
      private val fg    = new Color(cpt.foreground)

      def getPaint(value: Double): Paint = if (value.isNaN) fill else {
        val x = value * (cpt.maxValue - cpt.minValue) + cpt.minValue  // XXX TODO --- should no be normalized eventually
        if (pre == null) {
          val rgb = cpt.get(x)
          new Color(rgb)
        } else {
          val idx = cpt.indexOf(x)
          if (idx < 0) bg else if (idx >= pre.length) fg else pre(idx)
        }
      }
    }

    private lazy val intensityScale: PaintScale = {
      val res = new LookupPaintScale(0.0, 1.0, nanPaint /* Color.red */)
      val numM = IntensityPalette.numColors - 1
      for(i <- 0 to numM) {
        val d   = i.toDouble / numM
        val pnt = new Color(IntensityPalette.apply(d.toFloat))
        res.add(d, pnt)
      }
      res
    }

    private object mapOverlay extends AbstractOverlay with Overlay {
      import scala.concurrent.ExecutionContext.Implicits.global

      private var _enabled = false

      def enabled: Boolean = _enabled
      def enabled_=(value: Boolean): Unit = if (_enabled != value) {
        _enabled = value
        if (value) _main.addOverlay(mapOverlay) else _main.removeOverlay(mapOverlay)
      }

      private lazy val shapeFut: Future[Shape] = {
        val res = WorldMapOverlay()
        res.onSuccess {
          case _ => defer(fireOverlayChanged())
        }
        res.onFailure {
          case ex => ex.printStackTrace()
        }
        res
      }

      def paintOverlay(g2: Graphics2D, chartPanel: ChartPanel): Unit =
        for {
          opt   <- shapeFut.value
          shape <- opt
          xAxis <- Option(_plot.getDomainAxis)
          yAxis <- Option(_plot.getRangeAxis )
        } {
          val xRange  = xAxis.getRange
          val yRange  = yAxis.getRange
          val xMin    = xRange.getLowerBound
          val xMax    = xRange.getUpperBound
          val yMin    = yRange.getLowerBound
          val yMax    = yRange.getUpperBound
          // println(f"x = [$xMin%1.1f, $xMax%1.1f], y = [$yMin%1.1f, $yMax%1.1f]; x-size ${xDim.size}; y-size ${yDim.size}")
          import de.sciss.numbers.Implicits._
          val xVals   = _lastXVals
          val xSzM    = xVals.length - 1
          val yVals   = _lastYVals
          val ySzM    = yVals.length - 1
          if (xSzM >= 0 && ySzM >= 0) {
            val x0      = xVals(0)
            val xN      = xVals(xSzM)
            val y0      = yVals(0)
            val yN      = yVals(ySzM)
            val xMinD   = xMin.linlin(0, xSzM, x0, xN)
            val xMaxD   = xMax.linlin(0, xSzM, x0, xN)
            val yMinD   = yMin.linlin(0, ySzM, y0, yN)
            val yMaxD   = yMax.linlin(0, ySzM, y0, yN)

            // println(f"x = [$xMinD%1.1f, $xMaxD%1.1f], y = [$yMinD%1.1f, $yMaxD%1.1f]")

            val r         = chartPanel.getScreenDataArea
            val rw        = r.getWidth
            val rh        = r.getHeight
            val sxD       = xMaxD - xMinD
            val syD       = yMaxD - yMinD
            val ox        = -xMinD -180.0
            val oy        = yMaxD  - 90.0

            if (rw > 0 && rh > 0 && sxD > 0 && syD > 0) {
              val atOrig    = g2.getTransform
              val clipOrig  = g2.getClip
              g2.clip(r)
              g2.setColor(Color.gray)
              g2.translate(r.getMinX, r.getMinY)
              g2.scale(rw / sxD, rh / syD)
              g2.translate(ox, oy)
              g2.draw(shape)
              g2.setTransform(atOrig)
              g2.setClip(clipOrig)
            }
          }
        }
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
    private var _main: ChartPanel = _
    private var blockRenderer: XYBlockRenderer = _

    private def setPalette(name: Option[String]): Unit =
      blockRenderer.setPaintScale(mkScale(name.getOrElse("panoply")))

    private def guiInit(paletteValue0: Option[String]): Unit = {
      blockRenderer = new XYBlockRenderer()
      setPalette(paletteValue0)

      _plot = new XYPlot // (coll, xAxis, yAxis, renderer)
      _plot.setDataset(dataset)
      _plot.setRenderer(blockRenderer)

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

      overlayEmpty = new FlowPanel(
        new Label(null, raphael.Icon()(raphael.Shapes.GlobeEuropeAfrica), Alignment.Leading),
        viewOverlay.component
      )
      val panelOverlay = new OverlayPanel {
        contents += Swing.RigidBox(overlayEmpty.preferredSize)
        contents += overlayEmpty
      }

      val mColorTable = ComboBox.Model.wrap(ColorPaletteTable.builtIn.keys.toSeq.sortBy(_.toUpperCase))
      mColorTable.selectedItem = paletteValue0
      mColorTable.reactions += {
        case ComboBox.Model.SelectionChanged(_) =>
          val value = mColorTable.selectedItem
          val edit = cursor.step { implicit tx =>
            import StringEx.serializer
            val valueEx = value.map(StringEx.newConst[S])
            EditAttrMap.expr[S, String, StringElem](name = "Palette", obj = plotH(), key = Plot.attrPalette,
              value = valueEx)(StringElem.apply[S])
          }
          undoManager.add(edit)
      }
      val ggColorTable  = new ComboBox(mColorTable)

      val topPanel = new FlowPanel(panelOverlay, Swing.HStrut(8), new Label("Palette:"), ggColorTable)

      _main = new ChartPanel(chart, false)  // XXX TODO: useBuffer = false only during PDF export
      component = new BorderPanel {
        add(Component.wrap(_main), BorderPanel.Position.Center)
        add(topPanel             , BorderPanel.Position.South )
      }
    }
  }
}
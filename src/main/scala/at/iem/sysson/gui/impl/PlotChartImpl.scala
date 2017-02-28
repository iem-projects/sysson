/*
 *  PlotChartImpl.scala
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

import java.awt
import java.awt.image.BufferedImage
import java.awt.{Color, Graphics, Paint, Rectangle, Shape, TexturePaint}
import java.util.Locale
import javax.swing.Icon

import at.iem.sysson.gui.impl.AbstractPlotViewImpl.PlotData
import de.sciss.desktop.UndoManager
import de.sciss.equal
import de.sciss.icons.raphael
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, StringObj}
import de.sciss.lucre.matrix.gui.DimensionIndex
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.{BooleanCheckBoxView, CellView, DoubleSpinnerView, View, defer, deferTx}
import de.sciss.mellite.gui.AttrCellView
import de.sciss.mellite.gui.edit.EditAttrMap
import de.sciss.model.Model
import de.sciss.swingplus.{ComboBox, ListView, OverlayPanel}
import de.sciss.synth.proc.Workspace
import org.jfree.chart.axis.{NumberAxis, SymbolAxis}
import org.jfree.chart.panel.{AbstractOverlay, Overlay}
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.PaintScale
import org.jfree.chart.renderer.xy.XYBlockRenderer
import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.data.xy.{MatrixSeries, MatrixSeriesCollection}

import scala.concurrent.Future
import scala.swing.{Alignment, BorderPanel, Component, FlowPanel, Graphics2D, Label, Swing}
import scala.util.{Failure, Success}

object PlotChartImpl {
  def apply[S <: Sys[S]](plot: Plot[S], stats: PlotStatsView[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                                                 undoManager: UndoManager,
                                                                 workspace: Workspace[S]): View.Editable[S] = {
    new Impl[S](stats).init(plot)
  }

  private final class Impl[S <: Sys[S]](statsView: PlotStatsView[S])(implicit val cursor: stm.Cursor[S],
                                                                     val undoManager: UndoManager,
                                                                     val workspace: Workspace[S])
    extends AbstractPlotViewImpl[S] with View.Editable[S] {

    private[this] val dataset = new MatrixSeriesCollection

    private def calcMinMax(d: Array[Array[Float]], fill: Float): (Float, Float) = {
      var min  = Float.MaxValue
      var max  = Float.MinValue

      val fillIsNaN = fill.isNaN
      var i = 0
      while (i < d.length) {
        val d1 = d(i)
        var j = 0
        while (j < d1.length) {
          val f = d1(j)
          if (java.lang.Float.isInfinite(f)) sys.error(s"Unbounded value: $f")

          val isNaN = if (fillIsNaN) f.isNaN else f == fill
          if (!isNaN) {
            if (f < min) min = f
            if (f > max) max = f
          }
          j += 1
        }
        i += 1
      }
      (min, max)
    }

    private def scalaData(d: Array[Array[Float]], fill: Float, min: Float, max: Float): Array[Array[Float]] = {
      val div       = max - min
      if (div <= 0f) return d
      val mul       = 1f / div
      val res       = new Array[Array[Float]](d.length)
      val fillIsNaN = fill.isNaN

      var i = 0
      while (i < res.length) {
        val d1  = d(i)
        val d2  = new Array[Float](d1.length)
        res(i)  = d2
        // val d1 = res(i)
        var j = 0
        while (j < d1.length) {
          val f = d1(j)
          val isNaN = if (fillIsNaN) f.isNaN else f == fill
          d2(j) = if (isNaN) f else (f - min) * mul
          j += 1
        }
        i += 1
      }
      res
    }

    private[this] var _lastXName: String       = _
    private[this] var _lastYName: String       = _
    private[this] var _lastXVals: Array[Float] = new Array[Float](0)  // we use `sameElements`, so ensure it's not null
    private[this] var _lastYVals: Array[Float] = _lastXVals           // dito

    private[this] var _plotData: PlotData = _

    // called on EDT
    private def rescalePlot(): Unit = if (_plotData != null) updatePlot(_plotData)

    // called on EDT
    protected def updatePlot(data: PlotData): Unit = {
      _plotData = data

      dataset.removeAllSeries()
      val xName     = data.hName
      val xVals     = data.hData
      val xChanged  = xName != _lastXName || !(xVals sameElements _lastXVals)
      if (xChanged) {
        _lastXName  = xName
        _lastXVals  = xVals
        val xAxis   = createAxis(xName, data.hUnits, xVals)
        _plot.setDomainAxis(xAxis)
      }
      val yName     = data.vName
      val yVals     = data.vData
      val yChanged  = yName != _lastYName || !(yVals sameElements _lastYVals)
      if (yChanged) {
        _lastYName  = yName
        _lastYVals  = yVals
        val yAxis   = createAxis(yName, data.vUnits, yVals)
        _plot.setRangeAxis(yAxis)
      }

      // XXX TODO -- look for stats
      val fillValue = Float.NaN   // XXX TODO -- find fill value
      lazy val normMinMax = statsView.stats.fold(calcMinMax(data.mData, fillValue)) { stats =>
        val tot = stats.total
        (tot.min.toFloat, tot.max.toFloat)
      }

      val (minValue, maxValue) = if (normalize) normMinMax else {
        (minOpt.getOrElse(normMinMax._1.toDouble).toFloat,
         maxOpt.getOrElse(normMinMax._2.toDouble).toFloat)
      }

      val mData = scalaData(data.mData, fill = fillValue, min = minValue, max = maxValue)
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
        val xNameL    = xName.toLowerCase(Locale.US)
        val yNameL    = yName.toLowerCase(Locale.US)
        import equal.Implicits._
        isMap         = (xNameL === "lon" || xNameL === "longitude") && (yNameL === "lat" || yNameL === "latitude")
        updateOverlay()
      }
    }

    // ---- gui values ----

    private[this] var isMap     = false
    private[this] var minOpt    = Option.empty[Double]
    private[this] var maxOpt    = Option.empty[Double]
    private[this] var normalize = true

    // ----

    private[this] var viewOverlay     : BooleanCheckBoxView[S] = _
    private[this] var overlayEmpty    : Component = _
    private[this] var cellPalette     : CellView.Var[S, Option[String]] = _

    private[this] var minView         : DoubleSpinnerView.Optional[S] = _
    private[this] var maxView         : DoubleSpinnerView.Optional[S] = _
    private[this] var normalizeView   : BooleanCheckBoxView[S] = _

    // called on the EDT
    private def updateOverlay(): Unit = {
      overlayEmpty.visible  = isMap
      mapOverlay.enabled    = isMap && viewOverlay.component.selected
    }

    override def init(plot: Plot[S])(implicit tx: S#Tx): this.type = {
      super.init(plot)

      implicit val booleanEx  = BooleanObj
      implicit val stringEx   = StringObj
      implicit val doubleEx   = DoubleObj

      val plotAttr    = plot.attr
      val cellOverlay = AttrCellView[S, Boolean, BooleanObj](plotAttr, Plot.attrShowOverlay)
      viewOverlay     = BooleanCheckBoxView.optional(cellOverlay, name = "Map Overlay", default = false)
      addObserver(cellOverlay.react { implicit tx => _ => deferTx(updateOverlay()) })
      cellPalette     = AttrCellView[S, String, StringObj](plotAttr, Plot.attrPalette)
      addObserver(cellPalette.react { implicit tx => nameOpt =>
        deferTx {
          setPalette(nameOpt)
        }
      })

      val cellMin   = AttrCellView[S, Double , DoubleObj ](plotAttr, Plot.attrMin      )
      val cellMax   = AttrCellView[S, Double , DoubleObj ](plotAttr, Plot.attrMax      )
      val cellNorm  = AttrCellView[S, Boolean, BooleanObj](plotAttr, Plot.attrNormalize)
      minView       = DoubleSpinnerView  .optional(cellMin , name = "Minimum")
      maxView       = DoubleSpinnerView  .optional(cellMax , name = "Maximum")
      normalizeView = BooleanCheckBoxView.optional(cellNorm, name = "Normalize", default = true)

      addObserver(cellMin.react { implicit tx => value => deferTx {
        minOpt = value
        rescalePlot()
      }})

      addObserver(cellMax.react { implicit tx => value => deferTx {
        maxOpt = value
        rescalePlot()
      }})

      addObserver(cellNorm.react { implicit tx => value => deferTx {
        normalize = value.getOrElse(true)
        rescalePlot()
      }})

      val paletteValue0 = cellPalette()
      val minVal0       = cellMin()
      val maxVal0       = cellMax()
      val norm0         = cellNorm().getOrElse(true)

      deferTx(guiInit(paletteValue0 = paletteValue0, minVal0 = minVal0, maxVal0 = maxVal0, norm0 = norm0))

      this
    }

    override def dispose()(implicit tx: S#Tx): Unit = {
      super.dispose()
      deferTx {
        statsView.removeListener(statsListener)
      }
    }

    private[this] lazy val nanPaint: Paint = {
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

      private val cpt   = ColorPaletteTable.builtIn.getOrElse(name, ColorPaletteTable.fallBack) // problems in IntelliJ
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

    private[this] object mapOverlay extends AbstractOverlay with Overlay {
      import scala.concurrent.ExecutionContext.Implicits.global

      private var _enabled = false

      def enabled: Boolean = _enabled
      def enabled_=(value: Boolean): Unit = if (_enabled != value) {
        _enabled = value
        if (value) _main.addOverlay(mapOverlay) else _main.removeOverlay(mapOverlay)
      }

      private lazy val shapeFut: Future[Shape] = {
        val res = WorldMapOverlay()
        res.onComplete {
          case Success(_)  => defer(fireOverlayChanged())
          case Failure(ex) => ex.printStackTrace()
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

    private def createAxis(name: String, units: String, data: Array[Float]): NumberAxis = {
      import DimensionIndex.{mkUnitsString, shouldUseUnitsString, unitsStringFormatter}
      val nameC   = name.capitalize
      val lbUnits = shouldUseUnitsString(units)
      val nameU   = if (lbUnits) nameC else s"$nameC [${mkUnitsString(units)}]"
      val labels  = if (lbUnits) {
        val fmt = unitsStringFormatter(units)
        data.map(fmt(_))
      } else {
        data.map(_.toString)
      }
      val res = new SymbolAxis(nameU, labels)
      if (lbUnits) {
        // they might be long;
        // a 45 degree angle would be nicer, but requires
        // overring refreshTicksHorizontal/Vertical, cf. http://stackoverflow.com/questions/7071057
        res.setVerticalTickLabels(true)
        // res.setLabelAngle(math.Pi/4)
      }
      res.setStandardTickUnits(NumberAxis.createIntegerTickUnits())
      res.setLowerMargin(0.0)
      res.setUpperMargin(0.0)
      res
    }

    private[this] var _plot: XYPlot = _
    private[this] var _main: ChartPanel = _
    private[this] var blockRenderer: XYBlockRenderer = _

    private def setPalette(name: Option[String]): Unit =
      blockRenderer.setPaintScale(mkScale(name.getOrElse("panoply")))

    private[this] val statsListener: Model.Listener[Stats.Variable] = { case stats =>
      updateMinMaxDefaults(stats)
      if (normalize) rescalePlot()
    }

    private def guiInit(paletteValue0: Option[String], minVal0: Option[Double], maxVal0: Option[Double],
                        norm0: Boolean): Unit = {
      blockRenderer = new XYBlockRenderer()
      setPalette(paletteValue0)

      _plot = new XYPlot // (coll, xAxis, yAxis, renderer)
      _plot.setDataset(dataset)
      _plot.setRenderer(blockRenderer)

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

      val mColorTable = ComboBox.Model.wrap(ColorPaletteTable.builtIn.values.toSeq.sortBy(_.name.toUpperCase))
      mColorTable.selectedItem = paletteValue0.flatMap(ColorPaletteTable.builtIn.get)
      mColorTable.reactions += {
        case ComboBox.Model.SelectionChanged(_) =>
          val value = mColorTable.selectedItem.map(_.name)
          val edit = cursor.step { implicit tx =>
            val valueEx = value.map(StringObj.newConst[S])
            implicit val stringTpe = StringObj
            EditAttrMap.expr[S, String, StringObj](name = "Palette", obj = plotH(), key = Plot.attrPalette,
              value = valueEx) // (StringObj.apply[S])
          }
          undoManager.add(edit)
      }
      val ggColorTable  = new ComboBox(mColorTable)
      val rColorTable   = new ListView.Renderer[ColorPaletteTable] {
        private[this] var palette: ColorPaletteTable = _

        private[this] val icon = new Icon {
          def getIconWidth : Int = 64
          def getIconHeight: Int = 16

          def paintIcon(c: awt.Component, g: Graphics, x: Int, y: Int): Unit = {
            if (palette == null) return
            val paint = ColorPaletteTable.toPaint(palette, startX = x, endX = x + getIconWidth)
            val g2    = g.asInstanceOf[Graphics2D]
            g2.setPaint(paint)
            g2.fillRect(x, y, getIconWidth, getIconHeight)
          }
        }

        private[this] val component = new Label(null: String, icon, Alignment.Leading)
        component.opaque      = true
        component.iconTextGap = 6
        component.border      = Swing.EmptyBorder(2)

        def componentFor(list: ListView[_], sel: Boolean, focused: Boolean,
                         c: ColorPaletteTable, idx: Int): Component = {
          if (sel) {
            component.background = list.selectionBackground
            component.foreground = list.selectionForeground
          } else {
            component.background = list.background
            component.foreground = list.foreground
          }
          palette         = c
          component.text  = if (c == null) null else c.name
          component
        }
      }
      ggColorTable.renderer = rColorTable

      statsView.stats.foreach(updateMinMaxDefaults)
      statsView.addListener(statsListener)

      minOpt    = minVal0
      maxOpt    = maxVal0
      normalize = norm0

      val topPanel = new FlowPanel(panelOverlay, Swing.HStrut(8), new Label("Palette:"), ggColorTable,
        new Label("Min:"), minView.component, new Label("Max:"), maxView.component,
        normalizeView.component)

      _main = new ChartPanel(chart /* , false */)  // XXX TODO: useBuffer = false only during PDF export
      component = new BorderPanel {
        add(Component.wrap(_main), BorderPanel.Position.Center)
        add(topPanel             , BorderPanel.Position.South )
      }
    }

    private def updateMinMaxDefaults(stats: Stats.Variable): Unit = {
      val tot = stats.total
      minView.default = Some(tot.min)
      maxView.default = Some(tot.max)
    }
  }
}
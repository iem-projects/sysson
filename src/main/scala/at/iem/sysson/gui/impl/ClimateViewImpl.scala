package at.iem.sysson
package gui
package impl

import ucar.nc2
import org.jfree.chart.{JFreeChart, ChartPanel}
import Implicits._
import org.jfree.chart.renderer.xy.XYBlockRenderer
import org.jfree.chart.renderer.{PaintScale, LookupPaintScale, GrayPaintScale}
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.data.xy.{MatrixSeriesCollection, MatrixSeries}
import scala.Some
import java.awt.Color
import scala.swing.{CheckBox, BorderPanel, Component, Alignment, Slider, Label, Swing}
import Swing._
import scalaswingcontrib.group.GroupPanel
import collection.immutable.{IndexedSeq => IIdxSeq}
import javax.swing.{DefaultBoundedRangeModel, SpinnerNumberModel, JSpinner, GroupLayout}
import scala.swing.event.{ButtonClicked, ValueChanged}
import language.reflectiveCalls
import de.sciss.intensitypalette.IntensityPalette
import javax.swing.event.{ChangeEvent, ChangeListener}
import javax.swing.JSpinner.NumberEditor

object ClimateViewImpl {
  private class Reduction(val dim: Int, val norm: CheckBox, val name: Label, val slider: Slider,
                          val index: Component, val value: Label)

  private lazy val intensityScale: PaintScale = {
    val res = new LookupPaintScale(0.0, 1.0, Color.red)
    val numM = IntensityPalette.numColors - 1
    for(i <- 0 to numM) {
      val d   = i.toDouble / numM
      val pnt = new Color(IntensityPalette.apply(d.toFloat))
      res.add(d, pnt)
    }
    res
  }

  private final class MyMatrix(width: Int, height: Int) extends MatrixSeries("Climate", height, width) {
    def put(x: Int, y: Int, value: Float) {
      data(y)(x) = value
    }
  }

  private var _currentSection = Option.empty[VariableSection]

  def currentSection: Option[VariableSection] = _currentSection

  def apply(section: VariableSection): ClimateView = {
    val in = section.file

    var stats =  Option.empty[Stats.Variable]

    def valueFun(dim: nc2.Dimension, units: Boolean): Int => String =
      in.variableMap.get(dim.name.getOrElse("?")) match {
        case Some(v) if v.isFloat =>
          val dat = v.readSafe().float1D
          (i: Int) => f"${dat(i).toInt}%d${if (units) v.units.map(s => " " + s).getOrElse("") else ""}"

        case Some(v) if v.isDouble  =>
          val dat = v.readSafe().double1D
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

    val data   = new MyMatrix(width, height)

    //    @inline def time() = System.currentTimeMillis()

    def updateData() {
      // the indices in each reduction dimension currently selected
      val secIndices = redGUI.map(_.slider.value)

      // producing the following 2-dimensional section
      val sec = (red zip secIndices).foldLeft(section) { case (res, (d, idx)) =>
        res in d.name.getOrElse("?") select idx
      }
      // this variable can be read by the interpreter
      _currentSection = Some(sec)

      // read the raw data
      val arr0 = sec.readSafe().float1D

      // if the statistics are available, normalize according to checkboxes,
      // otherwise normalize across the current frame.
      val arr = stats match {
        case Some(s) =>
          // the names and indices of the dimensions which should be normalized
          val normDims = redGUI.collect {
            case r if r.norm.selected => red(r.dim).name.getOrElse("?") -> r.dim
          }
          // the minimum and maximum across the selected dimensions
          // (or total min/max if there aren't any dimensions checked)
          val (min, max) = if (normDims.isEmpty) s.total.min -> s.total.max else {
            val counts = normDims.map { case (name, idx) => s.slices(name)(secIndices(idx)) }
            counts match {
              case head +: tail => tail.foldLeft(head.min -> head.max) {
                case ((_min, _max), c) => math.max(_min, c.min) -> math.min(_max, c.max)
              }
            }
          }
          // println(s"min = $min, max = $max")
          arr0.linlin(min, max, sec.variable.fillValue)(0.0, 1.0)

        case _ => // no stats available yet, normalize current frame
          arr0.normalize(sec.variable.fillValue)
      }

      // fill the JFreeChart data matrix
      var x = 0; while(x < width) {
        var y = 0; while(y < height) {
          val z = arr(y * width + x)
          // data.setZValue(x, y, z)
          // data.update(y, x, z)
          data.put(x, y, z)
        y += 1 }
      x += 1 }
      // notify JFreeChart of the change, so it repaints the plot
      data.fireSeriesChanged()
    }

    // get the statistics from the cache manager
    import Stats.executionContext
    Stats.get(in).onSuccess {
      case Stats(map) => GUI.defer {
        // see if stats are available for the plotted variable
        val s = map.get(section.name)
        if (s.isDefined) {
          stats = s
          updateData()  // repaint with user defined normalization
          // enable the user definable normalization (checkboxes)
          redGUI.zipWithIndex.foreach { case (r, idx) =>
            r.norm.selected = false
            r.norm.enabled  = true
            r.norm.listenTo(r.norm)
            r.norm.reactions += {
              case ButtonClicked(_) => updateData()
            }
          }
        }
      }
    }

    lazy val redGUI: IIdxSeq[Reduction] = red.zipWithIndex.map { case (d, idx) =>
      val norm  = new CheckBox {
        enabled   = false
        selected  = true
        tooltip   = "Normalize using the selected slice"
      }
      val n     = d.name.getOrElse("?")
      val lb    = new Label(n + ":") {
        horizontalAlignment = Alignment.Trailing
        peer.putClientProperty("JComponent.sizeVariant", "small")
      }
      val m = valueFun(d, units = true)
      val dimMax = d.size - 1
      val curr = new Label {
        peer.putClientProperty("JComponent.sizeVariant", "small")
        text    = m(dimMax)
        val szm = preferredSize
        text    = m(0)
        val sz0 = preferredSize
        preferredSize = (math.max(sz0.width, szm.width), math.max(sz0.height, szm.height))
      }
      val spm = new SpinnerNumberModel(0, 0, dimMax, 1)
      val slm = new DefaultBoundedRangeModel(0, 1, 0, dimMax)
      val sl  = new Slider {
        peer.setModel(slm)
        //        min   = 0
        //        max   = dimMax
        //        value = 0
        peer.putClientProperty("JComponent.sizeVariant", "small")
        listenTo(this)
        reactions += {
          case ValueChanged(_) =>
            curr.text = m(value)

            if (!adjusting) updateData()
        }
      }

      slm.addChangeListener(new ChangeListener {
        def stateChanged(e: ChangeEvent) {
          spm.setValue(slm.getValue)
        }
      })
      spm.addChangeListener(new ChangeListener {
        def stateChanged(e: ChangeEvent) {
          slm.setValue(spm.getValue.asInstanceOf[Int])
        }
      })

      val spj = new JSpinner(spm) {
        putClientProperty("JComponent.sizeVariant", "small")
        // none of this works... stick to big font size then...
        //        val ed = new NumberEditor(this)
        //        ed.putClientProperty("JComponent.sizeVariant", "small")
        //        ed.setFont(new java.awt.Font("Times", java.awt.Font.PLAIN, 16))
        //        setEditor(ed)
      }
      val sp  = Component.wrap(spj)

      new Reduction(idx, norm, lb, sl, sp, curr)
    }

    val redGroup  = new GroupPanel {
      theHorizontalLayout is Sequential(
        Parallel(redGUI.map(r => add[GroupLayout#ParallelGroup](r.norm  )): _*),
        Parallel(redGUI.map(r => add[GroupLayout#ParallelGroup](r.name  )): _*),
        Parallel(redGUI.map(r => add[GroupLayout#ParallelGroup](r.slider)): _*),
        Parallel(redGUI.map(r => add[GroupLayout#ParallelGroup](r.index )): _*),
        Parallel(redGUI.map(r => add[GroupLayout#ParallelGroup](r.value )): _*)
      )

      theVerticalLayout is Sequential(
        (redGUI.map { r =>
          Parallel(Center)(r.norm, r.name, r.slider, r.index, r.value): InGroup[GroupLayout#SequentialGroup]
        }): _*
      )
    }

    updateData()

    val renderer  = new XYBlockRenderer()
    //    val scale     = new GrayPaintScale(0.0, 1.0)
    renderer.setPaintScale(intensityScale)
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
    val chart     = new JFreeChart(section.variable.description.getOrElse(section.variable.name), plot)
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
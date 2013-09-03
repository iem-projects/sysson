package at.iem.sysson
package gui
package impl

import ucar.nc2
import org.jfree.chart.{JFreeChart, ChartPanel}
import Implicits._
import org.jfree.chart.renderer.xy.XYBlockRenderer
import org.jfree.chart.renderer.{PaintScale, LookupPaintScale}
import org.jfree.chart.axis.{SymbolAxis, NumberAxis}
import org.jfree.chart.plot.XYPlot
import org.jfree.data.xy.{MatrixSeriesCollection, MatrixSeries}
import java.awt.Color
import scala.swing.{FlowPanel, TextField, Button, BoxPanel, Orientation, CheckBox, BorderPanel, Component, Alignment, Label, Swing}
import Swing._
import scalaswingcontrib.group.GroupPanel
import javax.swing.{TransferHandler, ImageIcon, SpinnerNumberModel, JSpinner, GroupLayout}
import scala.swing.event.{ButtonClicked, ValueChanged}
import language.reflectiveCalls
import de.sciss.intensitypalette.IntensityPalette
import javax.swing.event.{ChangeEvent, ChangeListener}
import javax.swing.TransferHandler.TransferSupport
import de.sciss.audiowidgets.{Transport, DualRangeModel, DualRangeSlider}
import at.iem.sysson.graph.{SelectedValue, SelectedRange}
import collection.breakOut
import de.sciss.desktop.OptionPane

object ClimateViewImpl {
  private class Reduction(val dim: Int, val norm: CheckBox, val name: Label, val slider: DualRangeSlider,
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
    def put(x: Int, y: Int, value: Float): Unit =
      data(y)(x) = value
  }

  private var _currentSection = Option.empty[VariableSection]

  def currentSection: Option[VariableSection] = _currentSection

  def apply(document: Document, section: VariableSection, xDim: nc2.Dimension, yDim: nc2.Dimension): ClimateView = {
    val in    = section.file
    val vm    = in.variableMap

    var stats = Option.empty[Stats.Variable]

    def valueFun(dim: nc2.Dimension, units: Boolean): Int => String =
      vm.get(dim.name) match {
        case Some(v) if v.isFloat =>
          val dat = v.readSafe().float1D
          (i: Int) => f"${dat(i).toInt}%d${if (units) v.units.map(s => " " + s).getOrElse("") else ""}"

        case Some(v) if v.isDouble  =>
          val dat = v.readSafe().double1D
          (i: Int) => f"${dat(i).toInt}%d${if (units) v.units.map(s => " " + s).getOrElse("") else ""}"

        case _ => (i: Int) => i.toString
      }

    //    val yDim     = section.dimensions.find { d =>
    //      d.name.flatMap(in.variableMap.get _).flatMap(_.units) == Some("degrees_north")
    //    } getOrElse sys.error("Did not find latitude dimension")
    //    val xDim     = section.dimensions.find { d =>
    //      d.name.flatMap(in.variableMap.get _).flatMap(_.units) == Some("degrees_east")
    //    } getOrElse sys.error("Did not find longitude dimension")

    val red     = section.reducedDimensions.filterNot(d => d == yDim || d == xDim)
    val width   = xDim.size
    val height  = yDim.size

    val data   = new MyMatrix(width, height)

    //    @inline def time() = System.currentTimeMillis()

    def updateData(): Unit = {
      // the indices in each reduction dimension currently selected
      val secIndices = redGUI.map(_.slider.value)

      // producing the following 2-dimensional section
      val sec = (red zip secIndices).foldLeft(section) { case (res, (d, idx)) =>
        res in d.name select idx
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
            case r if r.norm.selected => red(r.dim).name -> r.dim
          }
          // the minimum and maximum across the selected dimensions
          // (or total min/max if there aren't any dimensions checked)
          val (min, max) = if (normDims.isEmpty) s.total.min -> s.total.max else {
            val counts = normDims.flatMap { case (name, idx) => s.slices.get(name).map(_.apply(secIndices(idx))) }
            counts match {
              case head +: tail => tail.foldLeft(head.min -> head.max) {
                case ((_min, _max), c) => math.max(_min, c.min) -> math.min(_max, c.max)
              }
              case _ => s.total.min -> s.total.max
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

    lazy val redGUI: Vec[Reduction] = red.zipWithIndex.map { case (d, idx) =>
      val norm  = new CheckBox {
        enabled   = false
        selected  = true
        tooltip   = "Normalize using the selected slice"
      }
      val n     = d.name
      val lb    = new Label(n.capitalize + ":") {
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
      //      val slm = new DefaultBoundedRangeModel(0, 1, 0, dimMax)
      //      val sl  = new Slider {
      //        peer.setModel(slm)
      //        //        min   = 0
      //        //        max   = dimMax
      //        //        value = 0
      //        peer.putClientProperty("JComponent.sizeVariant", "small")
      //        listenTo(this)
      //        reactions += {
      //          case ValueChanged(_) =>
      //            curr.text = m(value)
      //
      //            if (!adjusting) updateData()
      //        }
      //      }
      val slm = DualRangeModel(0, dimMax)
      val sl  = new DualRangeSlider(slm) {
        rangeVisible = false  // becomes visible due to sonification mappings
        listenTo(this)
        reactions += {
          case ValueChanged(_) =>
            curr.text = m(value)
            /* if (!valueIsAdjusting) */ updateData()
        }
      }

      slm.addChangeListener(new ChangeListener {
        def stateChanged(e: ChangeEvent): Unit =
          spm.setValue(slm.value)
      })
      spm.addChangeListener(new ChangeListener {
        def stateChanged(e: ChangeEvent): Unit =
          slm.value = spm.getValue.asInstanceOf[Int]
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
        redGUI.map { r =>
          Parallel(Center)(r.norm, r.name, r.slider, r.index, r.value): InGroup[GroupLayout#SequentialGroup]
        }: _*
      )
    }

    updateData()

    val renderer  = new XYBlockRenderer()
    //    val scale     = new GrayPaintScale(0.0, 1.0)
    renderer.setPaintScale(intensityScale)

    def createAxis(dim: nc2.Dimension): NumberAxis = {
      val name  = dim.name
      val nameC = name.capitalize
      val res   = vm.get(name) match {
        case Some(v) if v.isFloat || v.isDouble =>
          val sz      = v.size.toInt
          val arr     = v.readSafe()
          val it      = arr.getIndexIterator
          val labels  = Array.fill[String](sz)(it.next().toString)
          new SymbolAxis(nameC, labels)

        case _ =>
          new NumberAxis(nameC)
      }
      res.setStandardTickUnits(NumberAxis.createIntegerTickUnits())
      res.setLowerMargin(0.0)
      res.setUpperMargin(0.0)
      res
    }

    val xAxis     = createAxis(xDim)
    val yAxis     = createAxis(yDim)
    val coll      = new MatrixSeriesCollection(data)
    val plot      = new XYPlot(coll, xAxis, yAxis, renderer)

    plot.setBackgroundPaint(Color.lightGray)
    plot.setDomainGridlinesVisible(false)
    plot.setRangeGridlinePaint(Color.white)
    val chart     = new JFreeChart(section.variable.description.getOrElse(section.variable.name), plot)
    chart.removeLegend()
    chart.setBackgroundPaint(Color.white)

    val models: Map[String, DualRangeSlider] = redGUI.map(r => red(r.dim).name -> r.slider)(breakOut)
    val view    = new Impl(document, section, models, chart, redGroup)
    view
  }

  private final class Impl(val document: Document, val section: VariableSection, models: Map[String, DualRangeSlider],
                           chart: JFreeChart, redGroup: Component)
    extends ClimateView {

    private val main        = new ChartPanel(chart)

    private val pSonif      = new BoxPanel(Orientation.Horizontal)
    private val ggSonifName = new TextField(16)
    ggSonifName.editable    = false
    ggSonifName.peer.putClientProperty("JComponent.sizeVariant", "small")

    private val transport = Transport.makeButtonStrip {
      import Transport._
      Seq(
        GoToBegin {
          rtz()
        },
        //        Rewind {
        //          println("Rewind")
        //        },
        Stop {
          stop()
        },
        Play {
          play()
          //        },
          //        FastForward {
          //          println("Fast-forward")
        }
      )
    }

    private val pSonif2     = new FlowPanel(ggSonifName, transport)
    pSonif2.visible         = false

    private val butSonif    = new Button(null: String)
    butSonif.icon           = new ImageIcon(Main.getClass.getResource("dropicon16.png"))
    butSonif.focusable      = false
    butSonif.tooltip        = "Drop Sonification Patch From the Library Here"

    pSonif.contents += butSonif
    pSonif.contents += pSonif2

    val component = new BorderPanel {
      add(Component.wrap(main), BorderPanel.Position.Center)
      add(new BorderPanel {
        add(redGroup, BorderPanel.Position.Center)
        add(pSonif  , BorderPanel.Position.South )
      }, BorderPanel.Position.South)
    }

    private var _patch = Option.empty[Patch]

    def play(): Unit = {
      println("Play")
    }

    def stop(): Unit = {
      println("Stop")
    }

    def rtz(): Unit = {
      println("Return-to-zero")
    }

    def patch: Option[Patch] = _patch
    def patch_=(value: Option[Patch]): Unit = {
      value match {
        case Some(p) =>
          ggSonifName.text  = p.name
          pSonif2.visible   = true
          val interactiveVars = p.graph.sources.collect {
            // case i: UserInteraction => i
            case SelectedRange(v) => v
            case SelectedValue(v) => v
          }
          // val docVars = document.data.variables
          val docVars = section.dimensions.flatMap(d => document.variableMap.get(d.name))
          val (foundVars, missingVars) = interactiveVars.map(v => v -> v.find(docVars).map(_.name))
            .partition(_._2.isDefined)
          if (missingVars.nonEmpty) {
            val msg = "The patch requires the following dimensions\nwhich are not part of this view:\n" +
              missingVars.map(_._1).mkString("\n  ", "\n  ", "")
            val opt = OptionPane.message(message = msg, messageType = OptionPane.Message.Error)
            opt.show(None)
          } else {
            val nameSet: Set[String] = foundVars.collect {
              case (_, Some(name)) => name
            } (breakOut)
            models.foreach { case (key, sli) =>
              sli.rangeVisible = nameSet.contains(key)
            }
          }

        case _ =>
          pSonif2.visible   = false
      }
      _patch = value
    }

    butSonif.peer.setTransferHandler(new TransferHandler {
      // how to enforce a drop action: https://weblogs.java.net/blog/shan_man/archive/2006/02/choosing_the_dr.html
      override def canImport(support: TransferSupport): Boolean = {
        val res =  if (support.isDataFlavorSupported(PatchFlavor) &&
           ((support.getSourceDropActions & TransferHandler.LINK) != 0)) {
          support.setDropAction(TransferHandler.LINK)
          true
        } else
          false

        // println(s"canImport? $res")
        res
      }

      override def importData(support: TransferSupport): Boolean = {
        val t       = support.getTransferable
        val data    = t.getTransferData(PatchFlavor).asInstanceOf[Patch]
        patch       = Some(data)
        true
      }
    })
  }
}
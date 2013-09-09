/*
 *  ClimateViewImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

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
import scala.swing.{TextField, Button, BoxPanel, Orientation, CheckBox, BorderPanel, Component, Alignment, Label, Swing}
import Swing._
import scalaswingcontrib.group.GroupPanel
import javax.swing.{TransferHandler, ImageIcon, SpinnerNumberModel, JSpinner, GroupLayout}
import scala.swing.event.{ButtonClicked, ValueChanged}
import language.reflectiveCalls
import de.sciss.intensitypalette.IntensityPalette
import javax.swing.event.{ChangeEvent, ChangeListener}
import javax.swing.TransferHandler.TransferSupport
import de.sciss.audiowidgets.{Transport, DualRangeModel, DualRangeSlider}
import at.iem.sysson.graph
import graph.{SelectedValue, SelectedRange}
import collection.breakOut
import de.sciss.desktop.{DialogSource, OptionPane}
import de.sciss.synth.{Ops, Synth}
import de.sciss.swingplus.Spinner
import scala.concurrent.{ExecutionContext, Future}
import at.iem.sysson.sound.Sonification
import scala.util.Success

object ClimateViewImpl {
  private class Reduction(val name: String, val dim: Int, val norm: CheckBox, val nameLabel: Label,
                          val slider: DualRangeSlider,
                          val index: Component, val valueLabel: Label)

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
            case r if r.norm.selected => r.name -> r.dim
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

    def mkReduction(dim: nc2.Dimension, idx: Int, update: Boolean): Reduction = {
      val norm  = new CheckBox {
        enabled   = false
        selected  = true
        tooltip   = "Normalize using the selected slice"
        if (!update) visible = false
      }
      val name  = dim.name
      val lb    = new Label(name.capitalize + ":") {
        horizontalAlignment = Alignment.Trailing
        peer.putClientProperty("JComponent.sizeVariant", "small")
      }
      val m       = valueFun(dim, units = true)
      val dimMax  = dim.size - 1
      val curr    = new Label {
        peer.putClientProperty("JComponent.sizeVariant", "small")
        text    = m(dimMax)
        val szm = preferredSize
        text    = m(0)
        val sz0 = preferredSize
        preferredSize = (math.max(sz0.width, szm.width), math.max(sz0.height, szm.height))
      }
      val spm = new SpinnerNumberModel(0, 0, dimMax, 1)
      val slm = DualRangeModel(0, dimMax)
      val sl  = new DualRangeSlider(slm) {
        rangeVisible = false  // becomes visible due to sonification mappings

        listenTo(this)
        reactions += {
          case ValueChanged(_) =>
            curr.text = m(value)
            if (update) /* if (!valueIsAdjusting) */ updateData()
        }

        // if (!update) valueVisible = false
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

      new Reduction(name, idx, norm, lb, sl, sp, curr)
    }

    lazy val redGUI: Vec[Reduction] = red.zipWithIndex.map { case (d, idx) =>
      mkReduction(d, idx, update = true)
    }

    // this is all pretty ugly XXX TODO
    val xDimRed = mkReduction(xDim, -1, update = false)
    val yDimRed = mkReduction(yDim, -1, update = false)

    lazy val redGUIAll = xDimRed +: yDimRed +: redGUI

    val redGroup  = new GroupPanel {
      theHorizontalLayout is Sequential(
        Parallel(redGUIAll.map(r => add[GroupLayout#ParallelGroup](r.norm      )): _*),
        Parallel(redGUIAll.map(r => add[GroupLayout#ParallelGroup](r.nameLabel )): _*),
        Parallel(redGUIAll.map(r => add[GroupLayout#ParallelGroup](r.slider    )): _*),
        Parallel(redGUIAll.map(r => add[GroupLayout#ParallelGroup](r.index     )): _*),
        Parallel(redGUIAll.map(r => add[GroupLayout#ParallelGroup](r.valueLabel)): _*)
      )

      theVerticalLayout is Sequential(
        redGUIAll.map { r =>
          Parallel(Center)(r.norm, r.nameLabel, r.slider, r.index, r.valueLabel): InGroup[GroupLayout#SequentialGroup]
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

    //    val xDimGUI = new FlowPanel(xDimRed.nameLabel, xDimRed.slider, xDimRed.index, xDimRed.valueLabel)
    //    val yDimGUI = new FlowPanel(yDimRed.nameLabel, yDimRed.slider, yDimRed.index, yDimRed.valueLabel)
    //    val dimPanel = new BoxPanel(Orientation.Vertical) {
    //      contents += xDimGUI
    //      contents += yDimGUI
    //      contents += redGroup
    //    }

    val models: Map[String, DualRangeSlider] = redGUIAll.map(r => r.name -> r.slider)(breakOut)

    val view    = new Impl(document, section, models, chart, /* dimPanel */ redGroup)
    view
  }

  private final class Impl(val document: Document, val section: VariableSection, models: Map[String, DualRangeSlider],
                           chart: JFreeChart, redGroup: Component)
    extends ClimateView {

    private val main        = new ChartPanel(chart)

    private val pSonif      = new BoxPanel(Orientation.Horizontal)
    private val ggSonifName = new TextField(16)
    ggSonifName.maximumSize = ggSonifName.preferredSize
    ggSonifName.editable    = false
    ggSonifName.peer.putClientProperty("JComponent.sizeVariant", "small")
    private val pUserValues = new BoxPanel(Orientation.Horizontal)  // cannot nest!: new FlowPanel()

    private var playing     = Option.empty[Future[Synth]]

    private var userValues  = Map.empty[String, SpinnerNumberModel]

    private val transport   = Transport.makeButtonStrip {
      import Transport._
      Seq(
        GoToBegin {
          rtz()
        },
        Stop {
          stop()
        },
        Play {
          play()
        }
      )
    }

    //private val pSonif2     = new FlowPanel(ggSonifName, transport, pUserValues)
    private val pSonif2     = new BoxPanel(Orientation.Horizontal) {
      contents += ggSonifName
      contents += transport
      contents += pUserValues
    }
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
      stop()
      markPlayStop(playing = true)
      patch.foreach { p =>
        val son          = Sonification(p.name)
        son.patch        = p
        son.variableMap += Sonification.DefaultVariable -> section
        models.foreach { case (key, model) =>
          val (start, end) = model.range
          val section = document.variableMap(key) in key select (start to end)
          son.variableMap += key -> section
        }
        userValues.foreach { case (key, model) =>
          son.userValueMap += key -> model.getValue.asInstanceOf[Double]
        }
        // println(s"sonfication.userValueMap = ${son.userValueMap}")

        import ExecutionContext.Implicits.global
        val fut          = son.prepare().map(_.play())
        playing          = Some(fut)

        def done(): Unit = GUI.defer {
          // only react if we're still talking about the same synth
          if (playing == Some(fut)) markPlayStop(playing = false)
        }

        fut.onComplete {
          case Success(synth) => synth.onEnd(done())
          case _              => done()
        }
        fut.onFailure {
          case ex: Exception =>
            DialogSource.Exception(ex -> s"Playing ${p.name}").show(None) // XXX TODO find window
          case f => f.printStackTrace()
        }
      }
    }

    private def markPlayStop(playing: Boolean) {
      transport.button(Transport.Stop).get.selected = !playing
      transport.button(Transport.Play).get.selected = playing
    }

    def stop(): Unit = {
      markPlayStop(playing = false)
      playing.foreach { fut =>
        import ExecutionContext.Implicits.global
        fut.onSuccess {
          case synth =>
            import Ops._
            synth.free()
        }
        playing = None
      }
    }

    def rtz(): Unit = {
      println("NOT YET IMPLEMENTED: Return-to-zero")
    }

    def patch: Option[Patch] = _patch
    def patch_=(value: Option[Patch]): Unit = {
      value match {
        case Some(p) =>
          ggSonifName.text    = p.name
          val sources         = p.graph.sources
          val interactiveVars = sources.collect {
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

          val _userValues = sources.collect {
            case graph.UserValue(key, default) =>
              val m = new SpinnerNumberModel(default, Double.MinValue, Double.MaxValue, 0.1)
              key -> m
          }

          userValues = _userValues.toMap

          // println(userValues.map(_._1).mkString(", "))

          _userValues.foreach { case (key, m) =>
            val lb = new Label(s"${key.capitalize}:")
            lb.peer.putClientProperty("JComponent.sizeVariant", "small")
            val spi = new Spinner(m)
            val d   = spi.preferredSize
            d.width = math.min(d.width, 80) // XXX TODO WTF
            spi.preferredSize = d
            spi.maximumSize   = d
            pUserValues.contents += HStrut(8)
            pUserValues.contents += lb
            pUserValues.contents += spi
          }

          pUserValues.contents += HGlue
          pUserValues.contents += HStrut(16)  // OS X resize gadget
          pSonif2.visible     = true
          pSonif2.revalidate()
          pSonif2.repaint()

        case _ =>
          pSonif2.visible   = false
          userValues        = Map.empty
          pUserValues.contents.clear()
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

    // ---- constructor ----
    markPlayStop(playing = false)
  }
}
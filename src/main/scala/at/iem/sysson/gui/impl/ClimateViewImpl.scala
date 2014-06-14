/*
 *  ClimateViewImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
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

import javax.swing.{JTable, SwingConstants, JSpinner, SpinnerNumberModel}

import ucar.nc2
import org.jfree.chart.{JFreeChart, ChartPanel}
import Implicits._
import org.jfree.chart.renderer.xy.XYBlockRenderer
import org.jfree.chart.renderer.{PaintScale, LookupPaintScale}
import org.jfree.chart.axis.{SymbolAxis, NumberAxis}
import org.jfree.chart.plot.{ValueMarker, IntervalMarker, XYPlot}
import org.jfree.data.xy.{MatrixSeriesCollection, MatrixSeries}
import java.awt.{BasicStroke, Color}
import scala.swing.{BoxPanel, Orientation, Alignment, BorderPanel, Table, CheckBox, Label, Component, Swing, Action}
import Swing._
import scala.swing.event.ValueChanged
import de.sciss.intensitypalette.IntensityPalette
import javax.swing.event.{ChangeEvent, ChangeListener}
import de.sciss.audiowidgets.{DualRangeModel, DualRangeSlider}
import collection.breakOut
import de.sciss.synth.SynthGraph
import de.sciss.swingplus.GroupPanel
import ucar.nc2.time.{CalendarPeriod, CalendarDateFormatter}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm
import scala.concurrent.stm.atomic
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{defer, deferTx, requireEDT}
import de.sciss.lucre.matrix.DataSource
import de.sciss.icons.raphael
import de.sciss.desktop.OptionPane
import javax.swing.table.DefaultTableCellRenderer
import scala.swing.event.ButtonClicked
import java.awt
import de.sciss.mellite.Workspace
import de.sciss.mellite.gui.GUI

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
    def put(x: Int, y: Int, value: Float): Unit = data(y)(x) = value
  }

  private var _currentSection = Option.empty[VariableSection]

  def currentSection: Option[VariableSection] = _currentSection

  def apply[S <: Sys[S]](document: DataSource[S], section: VariableSection, xDim: nc2.Dimension, yDim: nc2.Dimension)
           (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): ClimateView[S] = {
    // import WorkspaceResolver._
    implicit val resolver = WorkspaceResolver[S]
    val data  = document.data()
    val docH  = tx.newHandle(document)
    val view  = new Impl(docH, data, section, xDim, yDim)
    deferTx(view.guiInit())
    view
  }

  // XXX TODO: this should go somewhere else in a utility function
  private def mkUnitsString(v: nc2.Variable): Double => String =
    v.units.fold((d: Double) => f"$d%1.2f") {
      case "degrees_north"  => (d: Double) => if (d >= 0) f"$d%1.2f \u00B0N" else f"${-d}%1.2f \u00B0S"
      case "degrees_east"   => (d: Double) => if (d >= 0) f"$d%1.2f \u00B0E" else f"${-d}%1.2f \u00B0W"
      case "(0 - 1)"        => (d: Double) => f"${d * 100}%1.1f%%"
      case "kg m-2 s-1"     => (d: Double) => f"$d%1.2f kg/(m\u00B2s)"
      case "W m-2"          => (d: Double) => f"$d%1.2f W/m\u00B2"
      case "m s-1"          => (d: Double) => f"$d%1.2f m/s"
      case "Pa"             => (d: Double) => f"${d.toInt}%d Pa"
      case units            =>
        if (units.startsWith("days since")) {
          val date = CalendarDateFormatter.isoStringToCalendarDate(null, units.substring(11))
          (d: Double) => {
            val dt = date.add(d, CalendarPeriod.Field.Day)
            CalendarDateFormatter.toDateTimeString(dt)
          }

        } else if (units.startsWith("hours since")) {
          val date = CalendarDateFormatter.isoStringToCalendarDate(null, units.substring(12))
          (d: Double) => {
            val dt = date.add(d, CalendarPeriod.Field.Hour)
            CalendarDateFormatter.toDateTimeString(dt)
          }

        } else {
          (d: Double) => f"$d%1.2f $units"
        }
    }

  private def mkValueMarker(red: Reduction) = {
    val res = new ValueMarker(0, new Color(0x00, 0x00, 0xFF, 0x3F),
      new BasicStroke(1f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER, 2f, Array[Float](2f, 2f), 0f))
    red.slider.reactions += {
      case ValueChanged(_) =>
        val oldValue  = res.getValue
        val newValue  = red.slider.value
        if (newValue != oldValue) res.setValue(newValue)
    }
    res
  }

  private def mkRangeMarker(red: Reduction) = {
    val res = new IntervalMarker(0, 0, new Color(0x7F, 0x7F, 0x7F, 0x00), new BasicStroke(1f),
      new Color(0x00, 0x00, 0xFF, 0x00), new BasicStroke(1f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER, 2f,
        Array[Float](4f, 4f), 0f), 1f)
    var visible = false
    red.slider.reactions += {
      case ValueChanged(_) =>
        val newVisible = red.slider.rangeVisible
        if (visible != newVisible) {
          visible = newVisible
          res.setOutlinePaint(if (visible) new Color(0x00, 0x00, 0xFF, 0x3F) else new Color(0x00, 0x00, 0xFF, 0x00))
        }
        if (visible) {
          val oldStart            = res.getStartValue
          val oldEnd              = res.getEndValue
          val (newStart, newEnd)  = red.slider.range
          if (newStart != oldStart) res.setStartValue(newStart)
          if (newEnd   != oldEnd  ) res.setEndValue  (newEnd  )
        }
    }
    res
  }

  private final class Impl[S <: Sys[S]](docH: stm.Source[S#Tx, DataSource[S]], net: nc2.NetcdfFile, val section: VariableSection,
                           xDim: nc2.Dimension, yDim: nc2.Dimension)(implicit val workspace: Workspace[S], val cursor: stm.Cursor[S])
    extends ClimateView[S] with ComponentHolder[Component] {

    //    models: Map[String, DualRangeSlider],
    //    chart: JFreeChart, redGroup: Component

    private val red     = section.reducedDimensions.filterNot(d => d == yDim || d == xDim)
    private val width   = xDim.size
    private val height  = yDim.size

    private val in      = section.file
    private val vm      = in.variableMap

    private var stats   = Option.empty[Stats.Variable]

    private val data    = new MyMatrix(width, height)

    def dataSource(implicit tx: S#Tx): DataSource[S] = docH()

    def dispose()(implicit tx: S#Tx) = ()

    private def valueFun(dim: nc2.Dimension, units: Boolean): Int => String =
      vm.get(dim.name) match {
        case Some(v) if v.isFloat =>
          val dat = v.readSafe().float1D
          val f   = mkUnitsString(v)
          (i: Int) => f(dat(i))

        case Some(v) if v.isDouble  =>
          val dat = v.readSafe().double1D
          val f   = mkUnitsString(v)
          (i: Int) => f(dat(i))

        case _ => (i: Int) => i.toString
      }

    private def spawnStats(): Unit = {
      // get the statistics from the cache manager
      import Stats.executionContext
      atomic { implicit tx => Stats.get(in) } .onSuccess {
        case Stats(map) => defer {
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
            actionStats.enabled = s.isDefined
          }
        }
      }
    }

    private def mkReduction(dim: nc2.Dimension, idx: Int, update: Boolean): Reduction = {
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

    private def createAxis(dim: nc2.Dimension): NumberAxis = {
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

    private def updateData(): Unit = {
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

    private var models: Map[String, DualRangeSlider] = _

    private var redGUI: Vec[Reduction] = _

    // private var userValues  = Map.empty[String, SpinnerNumberModel]

    //    private var ggSonifName: TextField = _

    // private var playing     = Option.empty[Future[Synth]]

    // private var ggBusy: ProgressBar = _

    // private var transport: Component with Transport.ButtonStrip = _

    //    private var pUserValues: BoxPanel = _
    //
    //    private var pSonif2: BoxPanel = _

    private var actionStats: Action = _

    private def showStats(): Unit =
      stats.foreach { sv =>
        val tot = sv.total
        // println(tot)
        // val fmt = NumberFormat.getNumberInstance(Locale.US)
        // fmt.setMaximumFractionDigits(6)
        val tab = new Table(
          // use `.toString` for now, because default renderer applies some bad truncation
          Array[Array[Any]](
            Array("size"   , tot.num   ),
            Array("min"    , tot.min   ), // format tot.min   ),
            Array("max"    , tot.max   ), // format tot.max   ),
            Array("mean"   , tot.mean  ), // format tot.mean  ),
            Array("std-dev", tot.stddev)  // format tot.stddev)
          ),
          List("Key", "Value"))
        val colKey = tab.peer.getColumnModel.getColumn(0)
        colKey.setPreferredWidth(80)
        val colVal = tab.peer.getColumnModel.getColumn(1)
        tab.peer.setDefaultRenderer(classOf[java.lang.Double], new DefaultTableCellRenderer {
          setHorizontalAlignment(SwingConstants.RIGHT)

          private def formatValue(in: Any): Any = {
            // println("Aqui")
            if (in == null) return null
            // fmt.format(in)
            // better support for exponentials actually
            in match {
              case d: Double => d.toFloat.toString
              case _ => in
            }
          }

          override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean,
                                                     hasFocus: Boolean, row: Int,
                                                     column: Int): awt.Component =
            super.getTableCellRendererComponent(table, formatValue(value), isSelected, hasFocus, row, column)
        })
        colVal.setPreferredWidth(140)
        val opt   = OptionPane.message(message = tab, messageType = OptionPane.Message.Plain)
        opt.title = s"Statistics: ${sv.name}"
        opt.show(GUI.findWindow(component))
      }

    def guiInit(): Unit = {
      requireEDT()
      spawnStats()

      // this is all pretty ugly XXX TODO
      val xDimRed = mkReduction(xDim, -1, update = false)
      val yDimRed = mkReduction(yDim, -1, update = false)

      redGUI = red.zipWithIndex.map { case (d, idx) =>
        mkReduction(d, idx, update = true)
      }

      lazy val redGUIAll = xDimRed +: yDimRed +: redGUI

      val redGroup  = new GroupPanel {
        import GroupPanel.Element
        horizontal = Seq(
          Par(redGUIAll.map(_.norm      : Element): _*),
          Par(redGUIAll.map(_.nameLabel : Element): _*),
          Par(redGUIAll.map(_.slider    : Element): _*),
          Par(redGUIAll.map(_.index     : Element): _*),
          Par(redGUIAll.map(_.valueLabel: Element): _*)
        )

        vertical = Seq(
          redGUIAll.map { r =>
            Par(Center)(r.norm, r.nameLabel, r.slider, r.index, r.valueLabel)
          }: _*
        )
      }

      updateData()

      val renderer  = new XYBlockRenderer()
      //    val scale     = new GrayPaintScale(0.0, 1.0)
      renderer.setPaintScale(intensityScale)

      val xAxis     = createAxis(xDim)
      val yAxis     = createAxis(yDim)
      val coll      = new MatrixSeriesCollection(data)
      val plot      = new XYPlot(coll, xAxis, yAxis, renderer)

      val xmValue = mkValueMarker(xDimRed)
      val ymValue = mkValueMarker(yDimRed)
      val xmRange = mkRangeMarker(xDimRed)
      val ymRange = mkRangeMarker(yDimRed)

      plot.addDomainMarker(xmValue)
      plot.addRangeMarker (ymValue)
      plot.addDomainMarker(xmRange)
      plot.addRangeMarker (ymRange)

      plot.setBackgroundPaint(Color.lightGray)
      plot.setDomainGridlinesVisible(false)
      plot.setRangeGridlinePaint(Color.white)
      val chart     = new JFreeChart(section.variable.description.getOrElse(section.variable.name), plot)
      chart.removeLegend()
      chart.setBackgroundPaint(Color.white)

      models = redGUIAll.map(r => r.name -> r.slider)(breakOut)

      val main        = new ChartPanel(chart, false)  // XXX TODO: useBuffer = false only during PDF export

      val pButtons    = new BoxPanel(Orientation.Horizontal)
      //      ggSonifName     = new TextField(16)
      //      ggSonifName.maximumSize = ggSonifName.preferredSize
      //      ggSonifName.editable    = false
      //      ggSonifName.peer.putClientProperty("JComponent.sizeVariant", "small")
      //      pUserValues = new BoxPanel(Orientation.Horizontal)  // cannot nest!: new FlowPanel()
      actionStats = Action(null)(showStats())
      actionStats.enabled = stats.isDefined
      val ggStats = GUI.toolButton(actionStats, raphael.Shapes.InformationCircle, tooltip = "Show Statistics")
      pButtons.contents += ggStats
      pButtons.contents += HGlue

      //      transport   = Transport.makeButtonStrip {
      //        import Transport._
      //        Seq(
      //          GoToBegin {
      //            rtz()
      //          },
      //          Stop {
      //            stop()
      //          },
      //          Play {
      //            play()
      //          }
      //        )
      //      }

      // ggBusy = mkIndetProgress()

      //      pSonif2     = new BoxPanel(Orientation.Horizontal) {
      //        contents += ggSonifName
      //        contents += transport
      //        contents += pUserValues
      //      }
      //      pSonif2.visible         = false

      //      val butSonif    = new Button(null: String)
      //      butSonif.icon           = Icons.Target(24)
      //      butSonif.focusable      = false
      //      butSonif.tooltip        = "Drop Sonification Patch From the Library Here"

      //      pSonif.contents += butSonif
      //      pSonif.contents += new OverlayPanel {
      //        contents += RigidBox(ggBusy.preferredSize)
      //        contents += ggBusy
      //      }
      //      pSonif.contents += pSonif2

      //      butSonif.peer.setTransferHandler(new TransferHandler(null) {
      //        // how to enforce a drop action: https://weblogs.java.net/blog/shan_man/archive/2006/02/choosing_the_dr.html
      //        override def canImport(support: TransferSupport): Boolean = {
      //          val res =
      //            if (support.isDataFlavorSupported(DragAndDrop.LibraryNodeFlavor) &&
      //              ((support.getSourceDropActions & TransferHandler.LINK) != 0)) {
      //              support.setDropAction(TransferHandler.LINK)
      //              true
      //            } else
      //              false
      //
      //          // println(s"canImport? $res")
      //          res
      //        }
      //
      //        override def importData(support: TransferSupport): Boolean = {
      //          val t           = support.getTransferable
      //          // val source      = t.getTransferData(PatchSourceFlavor).asInstanceOf[Patch.Source]
      //          val drag      = t.getTransferData(DragAndDrop.LibraryNodeFlavor).asInstanceOf[LibraryNodeDrag]
      //          val sourceOpt = drag.cursor.step { implicit tx =>
      //            drag.node() match {
      //              case TreeLike.IsLeaf(l) => Some(l.name.value -> l.source.value)
      //              case _ => None: Option[(String, String)]
      //            }
      //          }
      //          sourceOpt.exists { case (name, source) =>
      //            import ExecutionContext.Implicits.global
      //            val fut         = Library.compile(source)
      //            ggBusy.visible  = true
      //            fut.onComplete(_ => defer { ggBusy.visible = false })
      //            fut.foreach { p => patch = Some(p) }
      //            true
      //          }
      //        }
      //      })

      component = new BorderPanel {
        add(Component.wrap(main), BorderPanel.Position.Center)
        add(new BorderPanel {
          add(redGroup, BorderPanel.Position.Center)
          add(pButtons  , BorderPanel.Position.South )
        }, BorderPanel.Position.South)
      }

      // ---- constructor ----
      // markPlayStop(playing = false)
    }

    //    private def mkIndetProgress() = new ProgressBar {
    //      visible       = false
    //      indeterminate = true
    //      preferredSize = (24, 24)
    //      peer.putClientProperty("JProgressBar.style", "circular")
    //    }

    private var _patch = Option.empty[SynthGraph]

    def play(): Unit = {
//      stop()
//      markPlayStop(playing = true)
//      patch.foreach { p =>
//        val son          = SonificationOLD(p.name)
//        son.patch        = p
//        son.variableMap += SonificationOLD.DefaultVariable -> section
//        models.foreach { case (key, model) =>
//          val (start, end) = model.range
//          val section = net.variableMap(key) in key select (start to end)
//          son.variableMap += key -> section
//        }
//        userValues.foreach { case (key, model) =>
//          son.userValueMap += key -> model.getValue.asInstanceOf[Double]
//        }
//        // println(s"sonfication.userValueMap = ${son.userValueMap}")
//
//        import ExecutionContext.Implicits.global
//        val fut          = son.prepare().map(_.play())
//        playing          = Some(fut)
//        ggBusy.visible = true
//
//        def done(): Unit = GUI.defer {
//          // only react if we're still talking about the same synth
//          if (playing == Some(fut)) markPlayStop(playing = false)
//        }
//
//        fut.onComplete {
//          case _ => GUI.defer(ggBusy.visible = false)
//        }
//        fut.onComplete {
//          case Success(synth) => synth.onEnd(done())
//          case _              => done()
//        }
//        fut.onFailure {
//          case ex: Exception =>
//            DialogSource.Exception(ex -> s"Playing ${p.name}").show(None) // XXX TODO find window
//          case f => f.printStackTrace()
//        }
//      }
    }

    //    private def markPlayStop(playing: Boolean): Unit = {
    //      transport.button(Transport.Stop).get.selected = !playing
    //      transport.button(Transport.Play).get.selected = playing
    //    }

    //    def stop(): Unit = {
    //      markPlayStop(playing = false)
    //      playing.foreach { fut =>
    //        import ExecutionContext.Implicits.global
    //        fut.onSuccess {
    //          case synth =>
    //            import Ops._
    //            synth.free()
    //        }
    //        playing = None
    //      }
    //    }
    //
    //    def rtz(): Unit = {
    //      println("NOT YET IMPLEMENTED: Return-to-zero")
    //    }

    def patch: Option[SynthGraph] = _patch
    def patch_=(value: Option[SynthGraph]): Unit = {
//      value match {
//        case Some(p) =>
//          ggSonifName.text    = p.name
//          val sources         = p.graph.sources
//          val interactiveVars = sources.collect {
//            // case i: UserInteraction => i
//            case SelectedRange(v) => v
//            case SelectedValue(v) => v
//          }
//          // val docVars = document.data.variables
//          val docVars = section.dimensions.flatMap(d => net.variableMap.get(d.name))
//          val (foundVars, missingVars) = interactiveVars.map(v => v -> v.find(docVars).map(_.name))
//            .partition(_._2.isDefined)
//          if (missingVars.nonEmpty) {
//            val msg = "The patch requires the following dimensions\nwhich are not part of this view:\n" +
//              missingVars.map(_._1).mkString("\n  ", "\n  ", "")
//            val opt = OptionPane.message(message = msg, messageType = OptionPane.Message.Error)
//            opt.show(None)
//          } else {
//            val nameSet: Set[String] = foundVars.collect {
//              case (_, Some(name)) => name
//            } (breakOut)
//            models.foreach { case (key, sli) =>
//              sli.rangeVisible = nameSet.contains(key)
//            }
//          }
//
//          val _userValues = sources.collect {
//            case graph.UserValue(key, default) =>
//              val m = new SpinnerNumberModel(default, Double.MinValue, Double.MaxValue, 0.1)
//              key -> m
//          }
//
//          userValues = _userValues.toMap
//
//          // println(userValues.map(_._1).mkString(", "))
//
//          _userValues.foreach { case (key, m) =>
//            val lb = new Label(s"${key.capitalize}:")
//            lb.peer.putClientProperty("JComponent.sizeVariant", "small")
//            val spi = new Spinner(m)
//            val d   = spi.preferredSize
//            d.width = math.min(d.width, 80) // XXX TODO WTF
//            spi.preferredSize = d
//            spi.maximumSize   = d
//            pUserValues.contents += HStrut(8)
//            pUserValues.contents += lb
//            pUserValues.contents += spi
//          }
//
//          pUserValues.contents += HGlue
//          pUserValues.contents += HStrut(16)  // OS X resize gadget
//          pSonif2.visible     = true
//          pSonif2.revalidate()
//          pSonif2.repaint()
//
//        case _ =>
//          pSonif2.visible   = false
//          userValues        = Map.empty
//          pUserValues.contents.clear()
//      }
      _patch = value
    }
  }
}
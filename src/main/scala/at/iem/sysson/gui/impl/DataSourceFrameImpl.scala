/*
 *  DataSourceFrameImpl.scala
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

import java.awt.Color
import javax.swing.SpinnerNumberModel
import javax.swing.undo.UndoableEdit

import at.iem.sysson.util.NetCdfFileUtil
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.{DialogSource, FileDialog, Menu, OptionPane, UndoManager, Window, WindowHandler}
import de.sciss.file._
import de.sciss.lucre.matrix.{DataSource, Matrix}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.{CellView, View, defer, deferTx, requireEDT}
import de.sciss.mellite.gui.impl.WindowImpl
import de.sciss.mellite.{Application, Workspace}
import de.sciss.serial.Serializer
import de.sciss.swingplus.Spinner
import de.sciss.{desktop, kollflitz, numbers}
import org.jfree.chart.ChartPanel
import org.jfree.chart.renderer.xy.XYStepAreaRenderer
import org.scalautils.TypeCheckedTripleEquals
import ucar.nc2

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.concurrent.stm.atomic
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.swing.{Alignment, BorderPanel, Button, Component, FlowPanel, GridPanel, Label, Orientation, SplitPane}
import scala.util.{Failure, Success}
import scalax.chart.{Chart, XYChart}

object DataSourceFrameImpl {
  def apply[S <: Sys[S]](source: DataSource[S])(implicit tx: S#Tx, workspace: Workspace[S],
                                                cursor: stm.Cursor[S]): DataSourceFrame[S] = {
    val view  = DataSourceView(source)
    val res   = new Impl(view)
    res.init()
    res
  }

  private class ConcatButtonImpl[S <: Sys[S]](set: => Unit)(implicit cursor: stm.Cursor[S],
                                                                    workspace: Workspace[S], undoManager: UndoManager)
    extends MatrixDnDViewImpl[S, Matrix](canSetMatrix = true, canRemoveMatrix = false) {

    private var _varOpt = Option.empty[nc2.Variable]

    def variableOption: Option[nc2.Variable] = {
      requireEDT()
      _varOpt
    }

    protected def matrix(m: Matrix[S])(implicit tx: S#Tx): Matrix[S] = m

    protected def editRemoveMatrix()(implicit tx: S#Tx): Option[UndoableEdit] = None

    protected def sourceSerializer: Serializer[S#Tx, S#Acc, Matrix[S]] = Matrix.serializer[S]

    protected def editDropMatrix(m: Matrix[S])(implicit tx: S#Tx): Option[UndoableEdit] = {
      updateSource(Some(m)) // XXX TODO --- sure this was not meant this way
      None
    }

    override def updateSource(mOpt: Option[Matrix[S]])(implicit tx: S#Tx): Unit = {
      super.updateSource(mOpt)

      implicit val wr = WorkspaceResolver[S]

      @tailrec def findVar(x: Matrix[S]): Option[nc2.Variable] = x match {
        case v: DataSource.Variable[S] => Some(v.data())
        case Matrix.Var(vr) => findVar(vr())
        case _ => None
      }

      val vOpt = mOpt.flatMap(findVar)
      deferTx {
        _varOpt = vOpt
        set
      }
    }
  }

  private def mkConcatWindow[S <: Sys[S]](w: Window, init1: stm.Source[S#Tx, Matrix[S]])(implicit cursor: stm.Cursor[S],
                                                               workspace: Workspace[S]): Unit = {
    implicit val undo = new UndoManagerImpl   // not used

    def bail(message: String): Unit =
      OptionPane.message(message = s"Cannot concatenate these matrices:\n$message",
        messageType = OptionPane.Message.Error).show(Some(w))

    lazy val ggProcess: Button = {
      val res = Button("Concatenate...") {
        for {
          v1 <- but1.variableOption
          v2 <- but2.variableOption
        } {
          import Implicits._
          val dim1 = v1.dimensions
          val dim2 = v2.dimensions
          import TypeCheckedTripleEquals._
          val sameDimNames = (dim1 zip dim2).forall { case (d1, d2) => d1.name === d2.name }
          val diffDimSizes = (dim1 zip dim2).count  { case (d1, d2) => d1.size !== d2.size }
          if (sameDimNames && diffDimSizes <= 1) {
            val (_, diff) = (dim1 zip dim2).partition { case (d1, d2) =>
              val arr1 = v1.file.variableMap(d1.name).readSafe()
              val arr2 = v2.file.variableMap(d2.name).readSafe()
              arr1.size == arr2.size && {
                val sz = arr1.size.toInt
                (0 until sz).forall { idx =>
                  arr1.getObject(idx) == arr2.getObject(idx)
                }
              }
            }
            if (diff.size == 1) {
              val dimName = diff.head._1.name
              withSaveFile(w, "Concatenated Output File") { out =>
                val proc = NetCdfFileUtil.concat(in1 = v1.file, in2 = v2.file, out = out, varName = v1.name, dimName = dimName)
                import ExecutionContext.Implicits.global
                proc.monitor(printResult = false)
                proc.onSuccess {
                  case _ => defer {
                    cursor.step { implicit tx => win.dispose() }
                  }
                }
                proc.start()
              }

            } else bail(s"All dimensions but one must be equal (found ${diff.size} dimensions that are different)")

          } else bail("Dimensions do not match")
        }
      }
      res.enabled = false
      res
    }

    def updateState(): Unit = {
      val ok = but1.variableOption.isDefined && but2.variableOption.isDefined
      ggProcess.enabled = ok
    }

    lazy val but1  = new ConcatButtonImpl[S](updateState())
    lazy val but2  = new ConcatButtonImpl[S](updateState())

    lazy val p = new BorderPanel {
      add(new GridPanel(2, 2) {
        contents += new Label("First Matrix:" , null, Alignment.Trailing)
        contents += but1.component
        contents += new Label("Second Matrix:", null, Alignment.Trailing)
        contents += but2.component
      }, BorderPanel.Position.North)

      add(ggProcess, BorderPanel.Position.South)
    }

    lazy val win = cursor.step { implicit tx =>
      but1.init()
      but2.init()

      but1.updateSource(Some(init1()))

      val _win = new WindowImpl[S](CellView.const("Concatenate Matrix")) {
        val view: View[S] = View.wrap[S](p)

        override def dispose()(implicit tx: S#Tx): Unit = {
          super.dispose()
          but1 .dispose()
          but2 .dispose()
        }

        override protected def performClose(): Unit =
          cursor.step { implicit tx => dispose() }  // yes, ugly. because we don't give a `View.Cursor
      }
      _win .init()
      _win
    }

    win
  }

  private def withSaveFile(w: Window, title: String)(fun: File => Unit): Unit =
    FileDialog.save(title = title).show(Some(w)).foreach { out0 =>
      val out = out0.replaceExt("nc")
      fun(out)
    }

  private final class Impl[S <: Sys[S]](val view: DataSourceView[S])
    extends WindowImpl[S] with DataSourceFrame[S] {

    private def withSelectedVariable(title: String)(fun: nc2.Variable => Unit): Unit =
      view.selectedVariable.fold[Unit] {
        OptionPane.message("Select a variable to process first.", OptionPane.Message.Error)
          .show(Some(window), title)
      } (fun)

    private def plot1D(): Unit = {
      import Implicits._
      withSelectedVariable(title) { vr =>
        if (vr.reducedRank != 1) {
          OptionPane.message(s"Variable ${vr.name} has ${vr.reducedRank} dimensions. Must have one.",
            OptionPane.Message.Error).show(Some(window), title = "Plot 1D")
        } else {
          import at.iem.sysson.Stats.executionContext
          val futData = Future {
            blocking {
              val dataV   = vr.readSafe().float1D
              val dimName = vr.reducedDimensions.head.name
              val vd      = vr.file.variableMap(dimName)
              val dataD   = vd.readSafe().float1D
              (dataV, dataD)
            }
          }
          futData.onComplete {
            case Success((dataV, dataD)) => defer {
              import scalax.chart.api._
              val data: Vec[(Float, Float)] = dataD zip dataV
              val dataCol = data.toXYSeriesCollection(title)
              val chart   = XYLineChart(dataCol, title = title, legend = false)
              ChartUtils.printableLook(chart)
              val plot    = chart.plot
              val renderer = plot.getRenderer
              renderer.setSeriesPaint(0, Color.darkGray)
              mkPlotWindow(mkChartPanel(chart), vr.name)
            }
            case Failure(ex) => defer {
              DialogSource.Exception(ex -> s"Plot of ${vr.name}").show(Some(window))
            }
          }
        }
      }
    }

    private def plotDist(): Unit = {
      import Implicits._
      withSelectedVariable(title) { vr =>
        val title0 = s"Distribution of ${vr.name}"

        import at.iem.sysson.Stats.executionContext
        val futStats = atomic { implicit tx =>
          Stats.get(vr.file)
        }
        val futHistoStats = futStats.map { case Stats(map) =>
          val total   = map(vr.name).total
          val numBins = 400 // XXX TODO -- could be configurable
          // val bins    = (0 until numBins).linlin(0, numBins - 1, total.min, total.max)
          val histo   = new Array[Int](numBins)

          def loop(rem: Vec[nc2.Dimension], red: VariableSection): Unit = rem match {
            case head +: (tail @ (_ +: _)) if red.size > 16384 =>   // try to make smart chunks
              for (i <- 0 until head.size) loop(tail, red.in(head.name).select(i))
            case _ =>
              val chunk = red.readSafe().float1D
              chunk.foreach { d =>
                import numbers.Implicits._
                val bin = d.linlin(total.min, total.max, 0, numBins).toInt.min(numBins - 1)
                histo(bin) += 1
              }
          }
          loop(vr.reducedDimensions, vr.selectAll)
          (histo, total)
        }
        println("Calculating... Please wait...")

        futHistoStats.onComplete {
          case Success((histo, total)) => defer {
            val size      = (0L /: histo)((sum, count) => sum + count)
            val relative: Vec[Double] = histo.map(count => (count * 100.0)/ size)(breakOut)
            import kollflitz.Ops._
            val accum     = relative.integrate

            //            println("----- HISTO -----")
            //            histo.zipWithIndex.foreach { case (d, i) =>
            //                println(f"$i%3d: $d%1.4f")
            //            }

            val chartRel    = mkHistoChart(relative , total, title = s"Histogram for ${vr.name}")
            val chartAccum  = mkHistoChart(accum    , total, title = s"Accumulative Histogram for ${vr.name}")
            mkPlotWindow(new SplitPane(Orientation.Vertical, mkChartPanel(chartRel  ), mkChartPanel(chartAccum)),
              title0)
          }

          case Failure(ex) => defer {
            DialogSource.Exception(ex -> s"Distribution of ${vr.name}").show(Some(window))
          }
        }
      }
    }

    private def mkChartPanel(chart: Chart): Component = Component.wrap(new ChartPanel(chart.peer))

    private def mkPlotWindow(chartPanel: Component, title0: String): Window =
      new desktop.impl.WindowImpl { win =>
        def handler: WindowHandler = Application.windowHandler
        contents = chartPanel
        title = title0
        pack()
        desktop.Util.centerOnScreen(this)
        front()
      }

    private def mkHistoChart(histo: Vec[Double], stats: Stats.Counts, title: String): XYChart = {
      import numbers.Implicits._

      import scalax.chart.api._
      val data: Vec[(Double, Double)] = histo.zipWithIndex.map { case (num, i) =>
        (i + 0.5).linlin(0, histo.length, stats.min, stats.max) -> num
      } (breakOut)
      val dataCol = data.toXYSeriesCollection(title)
      // val chart   = XYBarChart(dataCol, title = title, legend = false)
      val chart   = XYLineChart(dataCol, title = title, legend = false)
      ChartUtils.printableLook(chart)
      val plot    = chart.plot
      // val rangeX  = plot.getDomainAxis.asInstanceOf[NumberAxis]
      // val renderer = plot.getRenderer // .asInstanceOf[XYBarRenderer]
      val renderer = new XYStepAreaRenderer()
      plot.setRenderer(renderer)
      renderer.setSeriesPaint(0, Color.darkGray)
      // rangeX.setTickUnit(new NumberTickUnit(1))
      // val rangeY  = plot.getRangeAxis.asInstanceOf[NumberAxis]
      // rangeY.setTickUnit(new NumberTickUnit(1))
      // plot.getRenderer.asInstanceOf[BarRenderer].setMaximumBarWidth(0.1)
      // renderer.setMargin(1.0 - 1.0/histo.length) // (0.0)
      // val bp = new StandardXYBarPainter
      // renderer.setBarPainter(bp)
      chart
    }

    private def createAnomalies(): Unit = {
      val sw    = Some(window)
      val title = "Calculate Anomalies"
      withSelectedVariable(title) { vr =>
        import Implicits._

        if (!vr.dimensionMap.contains("time")) {
          OptionPane.message("Selected variable must have a dimension named 'time'", OptionPane.Message.Error)
            .show(sw, title)
        } else {
          val mYears    = new SpinnerNumberModel(30, 1, 10000, 1)
          val ggYears   = new Spinner(mYears)
          val pYears    = new FlowPanel(new Label("Average Years:"), ggYears)
          val lbInfo    = new Label(
            """<html><body>This process assumes that the time
              |dimension of the selected variable has a
              |<b>monthly resolution (12 values per year)</b>
              |with the first index corresponding to January.
              |
              |<b>A sliding window</b> of the size specified below
              |is used to to calculate the average values
              |per month and matrix cell. These sliding means
              |will be subtracted from the input matrix and
              |yield the "anomalies" output matrix.
              |</body>""".stripMargin.replace("\n", "<br>"))
          val pMessage  = new BorderPanel {
            add(lbInfo, BorderPanel.Position.Center)
            add(pYears, BorderPanel.Position.South )
          }
          val res = OptionPane(message = pMessage, optionType = OptionPane.Options.OkCancel).show(sw, title)
          import TypeCheckedTripleEquals._
          if (res === OptionPane.Result.Ok) {
            withSaveFile(window, "Anomalies Output File") { out =>
              val proc = NetCdfFileUtil.anomalies(in = vr.file, out = out, varName = vr.name, timeName = "time",
                windowYears = mYears.getNumber.intValue())
              import ExecutionContext.Implicits.global
              proc.monitor(printResult = false)
              proc.start()
            }
          }
        }
      }
    }

    private def concat(): Unit =
      view.mkSelectedMatrix().foreach { varH =>
        import view.{cursor, workspace}
        mkConcatWindow[S](window, varH)
      }

    override protected def initGUI(): Unit = {
      val root  = window.handler.menuFactory
      val path  = "actions"
      root.get(path) match {
        case Some(g: Menu.Group) =>
          val sw = Some(window)
          g.add(sw, Menu.Item("plot-1d"          )("Plot 1D..."            )(plot1D()))
          g.add(sw, Menu.Item("plot-distribution")("Plot Distribution..."  )(plotDist()))
          g.add(sw, Menu.Item("create-anomalies" )("Calculate Anomalies...")(createAnomalies()))
          g.add(sw, Menu.Item("create-concat"    )("Concatenate Matrix..." )(concat()))

        case _ => sys.error(s"No menu group for path '$path'")
      }
    }
  }
}

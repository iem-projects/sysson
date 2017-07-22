/*
 *  ActionCalculateAnomalies.scala
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

import javax.swing.SpinnerNumberModel

import at.iem.sysson.util.NetCdfFileUtil
import de.sciss.audiowidgets.DualRangeModel
import de.sciss.desktop.{FileDialog, OptionPane, UndoManager, Window}
import de.sciss.equal
import de.sciss.file._
import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.matrix.gui.DimensionIndex
import de.sciss.lucre.matrix.gui.impl.ReductionView.UnitLabelImpl
import de.sciss.lucre.matrix.{DataSource, Dimension, Matrix, Reduce, Sys}
import de.sciss.lucre.swing.{IntRangeSliderView, IntSpinnerView, View}
import de.sciss.lucre.{matrix, stm}
import de.sciss.swingplus.{ComboBox, GroupPanel, Spinner}
import de.sciss.synth.proc.{GenContext, WorkspaceHandle}
import ucar.nc2

import scala.concurrent.ExecutionContext
import scala.swing.{Action, BorderPanel, BoxPanel, Component, Label, Orientation, Swing}

final class ActionCalculateAnomalies[I <: Sys[I]](windowOpt: Option[Window], selectedVariable: => Option[nc2.Variable])
                                                 (implicit cursor: stm.Cursor[I])
  extends Action("Calculate Anomalies...") {

  private def mkTimeRangeSelector(vr: nc2.Variable, timeName: String): (Component, DualRangeModel) = {
    // XXX TODO --- yes, it's messy, and DRY
    val (_view, _rm) = cursor.step { implicit tx =>
      import Implicits._
      import matrix.Implicits._

      implicit val resolver: DataSource.Resolver[I] = DataSource.Resolver.empty
      implicit val ws   : WorkspaceHandle[I]        = WorkspaceHandle.Implicits.dummy
      implicit val undo : UndoManager               = UndoManager()
      implicit val gen  : GenContext[I]             = GenContext[I]
      import ExecutionContext.Implicits.global

      val dimMat = vr.file.variableMap.get(timeName).fold {
        Matrix.newConst1D[I]("dim0", Vector(0.0), "")
      } { dim =>
        val dimVals = dim.readSafe().double1D
        val unitsOpt = dim.units
        Matrix.newConst1D[I]("dim0", dimVals, unitsOpt.getOrElse(""))
      }
      val dimSize   = dimMat.size.toInt
      val dimVal    = Dimension.Value(timeName, dimSize)
      val os        = Reduce.Op.Slice[I](IntObj.newVar(0), IntObj.newVar(dimSize))
      val dimRange  = dimMat.ranges.apply(0)

      val rm        = DualRangeModel(minimum = 0, maximum = dimVal.size - 1)
      val nameSlice = s"Slice in ${dimVal.name}"
      val viewSlice = IntRangeSliderView[I](rm, nameSlice)
      val exprLo    = os.from
      val exprHi    = os.to
      viewSlice.rangeLo = Some(exprLo)
      viewSlice.rangeHi = Some(exprHi)
      val spinLo    = IntSpinnerView(exprLo, nameSlice, 80)
      val spinHi    = IntSpinnerView(exprHi, nameSlice, 80)
      val dimIdxView = DimensionIndex(dimMat)
      val unitLo    = new UnitLabelImpl(dimIdxView, dimRange).init(exprLo)
      val unitHi    = new UnitLabelImpl(dimIdxView, dimRange).init(exprHi)
      val view: View[I] = new View[I] {
        lazy val component: Component = new BoxPanel(Orientation.Horizontal) {
          contents += viewSlice.component
          contents += new BoxPanel(Orientation.Vertical) {
            contents += new BoxPanel(Orientation.Horizontal) {
              contents += spinLo.component
              contents += unitLo.component
            }
            contents += new BoxPanel(Orientation.Horizontal) {
              contents += spinHi.component
              contents += unitHi.component
            }
          }
          contents += Swing.HStrut(4)
        }

        def dispose()(implicit tx: I#Tx): Unit = {
          viewSlice .dispose()
          spinLo    .dispose()
          spinHi    .dispose()
          unitLo    .dispose()
          unitHi    .dispose()
          dimIdxView.dispose()
        }
      }
      (view, rm)
    }
    (_view.component, _rm)
  }

  def apply(): Unit = {
    val title = "Calculate Anomalies"
    selectedVariable.foreach { vr =>
      import Implicits._

      val timeNameOpt = vr.dimensionMap.keySet.find(_.toLowerCase == "time")
      timeNameOpt.fold[Unit] {
        OptionPane.message("Selected variable must have a dimension named 'time'", OptionPane.Message.Error)
          .show(windowOpt, title)
      } { timeName =>
        val mYears    = new SpinnerNumberModel(30, 1, 10000, 1)
        val ggYears   = new Spinner(mYears)
        val ggAvg     = new ComboBox(Seq("Mean", "Median"))

        val (ggTimeRange, timeRangeM) = mkTimeRangeSelector(vr, timeName)

        val lbTimeRange = new Label("Time Range:")
        val lbYears     = new Label("Average Years:")
        val lbAvg       = new Label("Averaging Mode:")

        val pYears = new GroupPanel {
          horizontal = Seq(
            Par(lbTimeRange, lbYears, lbAvg),
            Par(ggTimeRange, ggYears, Seq(ggAvg, Gap.Spring()))  // XXX TODO --- not exactly what we want
          )

          vertical = Seq(
            Par(Baseline)(lbTimeRange , ggTimeRange ),
            Par(Baseline)(lbYears     , ggYears     ),
            Par(Baseline)(lbAvg       , ggAvg       )
          )

          border = Swing.EmptyBorder(8, 0, 8, 0)
        }

        val lbInfo    = new Label(
          """<html><body>This process assumes that the time dimension
            |of the selected variable has a <b>monthly resolution
            |(12 values per year)</b>.
            |
            |<b>A sliding window</b> of the size specified in years
            |and clipped to a given time range is used to to calculate
            |the average values per month and matrix cell. These
            |sliding means will be subtracted from the input matrix
            |and yield the "anomalies" output matrix.
            |</body>""".stripMargin.replace("\n", "<br>"))
        val pMessage  = new BorderPanel {
          add(lbInfo, BorderPanel.Position.Center)
          add(pYears, BorderPanel.Position.South )
        }
        val res = OptionPane(message = pMessage, optionType = OptionPane.Options.OkCancel).show(windowOpt, title)
        import equal.Implicits._
        if (res === OptionPane.Result.Ok) {
          withSaveFile("Anomalies Output File") { out =>
            val proc = NetCdfFileUtil.anomalies(in = vr.file, out = out, varName = vr.name, timeName = timeName,
              windowYears = mYears.getNumber.intValue(), useMedian = ggAvg.selection.item === "Median",
              timeRangeOpt = Some((timeRangeM.rangeLo, timeRangeM.rangeHi + 1))
            )
            import ExecutionContext.Implicits.global
            proc.monitor(printResult = false)
            proc.start()
          }
        }
      }
    }
  }

  private def withSaveFile(title: String)(fun: File => Unit): Unit =
    FileDialog.save(title = title).show(windowOpt).foreach { out0 =>
      val out = out0.replaceExt("nc")
      fun(out)
    }
}

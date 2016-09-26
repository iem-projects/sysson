/*
 *  ActionCalculateAnomalies.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2016 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2016 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.desktop.{FileDialog, OptionPane, Window}
import de.sciss.equal
import de.sciss.file._
import de.sciss.swingplus.Spinner
import ucar.nc2

import scala.concurrent.ExecutionContext
import scala.swing.{Action, BorderPanel, FlowPanel, Label}

final class ActionCalculateAnomalies(windowOpt: Option[Window], selectedVariable: => Option[nc2.Variable])
  extends Action("Calculate Anomalies...") {
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
        val pYears    = new FlowPanel(new Label("Average Years:"), ggYears)
        val lbInfo    = new Label(
          """<html><body>This process assumes that the time
            |dimension of the selected variable has a
            |<b>monthly resolution (12 values per year)</b>.
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
        val res = OptionPane(message = pMessage, optionType = OptionPane.Options.OkCancel).show(windowOpt, title)
        import equal.Implicits._
        if (res === OptionPane.Result.Ok) {
          withSaveFile("Anomalies Output File") { out =>
            val proc = NetCdfFileUtil.anomalies(in = vr.file, out = out, varName = vr.name, timeName = timeName,
              windowYears = mYears.getNumber.intValue())
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

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

import javax.swing.SpinnerNumberModel

import at.iem.sysson.util.NetCdfFileUtil
import de.sciss.desktop.{FileDialog, Menu, OptionPane}
import de.sciss.file._
import de.sciss.lucre.event.Sys
import de.sciss.lucre.matrix.DataSource
import de.sciss.lucre.stm
import de.sciss.mellite.Workspace
import de.sciss.mellite.gui.impl.WindowImpl
import de.sciss.swingplus.Spinner
import ucar.nc2

import scala.concurrent.ExecutionContext
import scala.swing.{FlowPanel, Label}

object DataSourceFrameImpl {
  def apply[S <: Sys[S]](source: DataSource[S])(implicit tx: S#Tx, workspace: Workspace[S],
                                                cursor: stm.Cursor[S]): DataSourceFrame[S] = {
    val view  = DataSourceView(source)
    val res   = new Impl(view)
    res.init()
    res
  }

  private def mkConcatWindow(init1: nc2.Variable): Unit = {
    ???
  }

  private final class Impl[S <: Sys[S]](val view: DataSourceView[S])
    extends WindowImpl[S] with DataSourceFrame[S] {

    private def withSelectedVariable(title: String)(fun: nc2.Variable => Unit): Unit =
      view.selectedVariable.fold[Unit] {
        OptionPane.message("Select a variable to process first.", OptionPane.Message.Error)
          .show(Some(window), title)
      } (fun)

    private def withSaveFile(title: String)(fun: File => Unit): Unit =
      FileDialog.save(title = "Anomalies Output File").show(Some(window)).foreach { out0 =>
        val out = out0.replaceExt("nc")
        fun(out)
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
          val mYears  = new SpinnerNumberModel(30, 1, 10000, 1)
          val ggYears = new Spinner(mYears)
          val message = new FlowPanel(new Label("Average Years:"), ggYears)
          val res = OptionPane(message = message, optionType = OptionPane.Options.OkCancel).show(sw, title)
          if (res == OptionPane.Result.Ok) {
            withSaveFile(title) { out =>
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

    private def concat(): Unit = {
      val sw    = Some(window)
      val title = "Concatenate Matrix"
      withSelectedVariable(title) { vr =>
        mkConcatWindow(vr)
      }
    }

    override protected def initGUI(): Unit = {
      val root  = window.handler.menuFactory
      val path  = "actions"
      root.get(path) match {
        case Some(g: Menu.Group) =>
          val sw = Some(window)
          g.add(sw, Menu.Item("create-anomalies")("Calculate Anomalies...")(createAnomalies()))
          g.add(sw, Menu.Item("create-concat"   )("Concatenate Matrix..." )(concat()))

        case _ => sys.error(s"No menu group for path '$path'")
      }
    }
  }
}

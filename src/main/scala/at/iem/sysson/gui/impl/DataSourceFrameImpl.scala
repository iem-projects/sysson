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
import javax.swing.undo.UndoableEdit

import at.iem.sysson.util.NetCdfFileUtil
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.{Window, FileDialog, Menu, OptionPane, UndoManager}
import de.sciss.file._
import de.sciss.lucre.event.Sys
import de.sciss.lucre.matrix.{DataSource, Matrix}
import de.sciss.lucre.stm
import de.sciss.lucre.swing.{CellView, View, defer, deferTx, requireEDT}
import de.sciss.mellite.Workspace
import de.sciss.mellite.gui.impl.WindowImpl
import de.sciss.serial.Serializer
import de.sciss.swingplus.Spinner
import ucar.nc2

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.swing.{Alignment, BorderPanel, Button, FlowPanel, GridPanel, Label}

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
          val sameDimNames = (dim1 zip dim2).forall { case (d1, d2) => d1.name == d2.name }
          val diffDimSizes = (dim1 zip dim2).count  { case (d1, d2) => d1.size != d2.size }
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
          g.add(sw, Menu.Item("create-anomalies")("Calculate Anomalies...")(createAnomalies()))
          g.add(sw, Menu.Item("create-concat"   )("Concatenate Matrix..." )(concat()))

        case _ => sys.error(s"No menu group for path '$path'")
      }
    }
  }
}

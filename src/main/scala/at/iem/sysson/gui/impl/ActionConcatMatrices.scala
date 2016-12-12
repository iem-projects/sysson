/*
 *  ActionConcatMatrices.scala
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

import javax.swing.undo.UndoableEdit

import at.iem.sysson.util.NetCdfFileUtil
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.{FileDialog, OptionPane, UndoManager, Window}
import de.sciss.equal
import de.sciss.file._
import de.sciss.lucre.matrix.{DataSource, Matrix}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.{CellView, View, _}
import de.sciss.mellite.gui.impl.WindowImpl
import de.sciss.serial.Serializer
import de.sciss.synth.proc.Workspace
import ucar.nc2

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.swing.{Action, Alignment, BorderPanel, Button, GridPanel, Label}

final class ActionConcatMatrices[S <: Sys[S]](windowOpt: Option[Window], view: DataSourceView[S])
  extends Action("Concatenate Matrix...") {

  def apply(): Unit =
    view.mkSelectedMatrix().foreach { varH =>
      mkConcatWindow(varH)
    }

  private def mkConcatWindow(init1: stm.Source[S#Tx, Matrix[S]]): Unit = {
    import view.cursor
    implicit val undo = new UndoManagerImpl // not used

    def bail(message: String): Unit =
      OptionPane.message(message = s"Cannot concatenate these matrices:\n$message",
        messageType = OptionPane.Message.Error).show(windowOpt)

    lazy val ggProcess: Button = {
      val res = Button("Concatenate...") {
        for {
          v1 <- but1.variableOption
          v2 <- but2.variableOption
        } {
          import Implicits._
          val dim1 = v1.dimensions
          val dim2 = v2.dimensions
          import equal.Implicits._
          val sameDimNames = (dim1 zip dim2).forall { case (d1, d2) => d1.name === d2.name }
          val diffDimSizes = (dim1 zip dim2).count { case (d1, d2) => d1.size !== d2.size }
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
              withSaveFile("Concatenated Output File") { out =>
                val proc = NetCdfFileUtil.concat(in1 = v1.file, in2 = v2.file, out = out,
                  varName = v1.name, dimName = dimName)
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

    import view.workspace // XXX IntelliJ highlighting bug; this is needed!
    lazy val but1 = new ConcatButtonImpl(updateState())
    lazy val but2 = new ConcatButtonImpl(updateState())

    lazy val p = new BorderPanel {
      add(new GridPanel(2, 2) {
        contents += new Label("First Matrix:", null, Alignment.Trailing)
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
          but1.dispose()
          but2.dispose()
        }

        override protected def performClose(): Unit =
          cursor.step { implicit tx => dispose() } // yes, ugly. because we don't give a `View.Cursor
      }
      _win.init()
      _win
    }

    win
  }

  private def withSaveFile(title: String)(fun: File => Unit): Unit =
    FileDialog.save(title = title).show(windowOpt).foreach { out0 =>
      val out = out0.replaceExt("nc")
      fun(out)
    }

  private class ConcatButtonImpl(set: => Unit)(implicit cursor: stm.Cursor[S],
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
}
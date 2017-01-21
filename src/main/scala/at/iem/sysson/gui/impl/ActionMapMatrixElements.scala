/*
 *  ActionMapMatrixElements.scala
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

import at.iem.sysson.util.{DoubleTransform, NetCdfFileUtil}
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.{FileDialog, OptionPane, Window}
import de.sciss.file._
import de.sciss.processor
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.{CellView, View}
import de.sciss.mellite.Mellite
import de.sciss.mellite.gui.CodeView
import de.sciss.mellite.gui.impl.WindowImpl
import de.sciss.synth.proc.Code
import ucar.{ma2, nc2}

import scala.concurrent.ExecutionContext
import scala.swing.{Action, BorderPanel, Button}
import scala.util.{Failure, Success}

final class ActionMapMatrixElements[S <: Sys[S]](windowOpt: Option[Window], view: DataSourceView[S])
  extends Action("Transform Matrix Elements...") {

  private[this] val title0 = "Transform Matrix Elements"

  def bail(message: String): Unit =
    OptionPane.message(message = s"Cannot transform this matrix:\n$message",
      messageType = OptionPane.Message.Error).show(windowOpt)

  def apply(): Unit = {
    import view.{cursor, workspace}
    view.selectedVariable.foreach { vr =>
      lazy val win = cursor.step { implicit tx =>
        val code0         = DoubleTransform.Code("x")
        val codeObj       = Code.Obj.newVar(Code.Obj.newConst[S](code0))
        import Mellite.compiler
        implicit val undo = new UndoManagerImpl

        val viewTransform = View.wrap[S] {
          Button("Transform...") {
            cursor.step { implicit tx =>
              codeObj.value
            } match {
              case source @ DoubleTransform.Code(_) =>
                import ExecutionContext.Implicits.global
                DoubleTransform.compile(source).onComplete {
                  case Success(dt) =>
                    run(vr, dt)
                  case Failure(ex) =>
                    ex.printStackTrace()
                    bail(s"Compile error - ${ex.getMessage}")
                }
              case other => bail(s"Not a DoubleTransform - $other")
            }
          }
        }

        val _codeView     = CodeView(obj = codeObj, code0 = code0, bottom = viewTransform :: Nil)(None)

        lazy val p = new BorderPanel {
          _codeView.component.preferredSize = {
            val dim = _codeView.component.preferredSize
            dim.height = math.min(100, dim.height)
            dim
          }
          add(_codeView.component, BorderPanel.Position.Center)
        }

        val _win = new WindowImpl[S](CellView.const(title0)) {
          val view: View[S] = View.wrap[S](p)

          override def dispose()(implicit tx: S#Tx): Unit = {
            super.dispose()
            _codeView.dispose()
          }

          override protected def performClose(): Unit =
            cursor.step { implicit tx => dispose() } // yes, ugly. because we don't give a `View.Cursor
        }
        _win.init()
        _win
      }

      win
    }
  }

  def run(vr: nc2.Variable, dt: DoubleTransform): Unit =
    withSaveFile("Transformed Output File") { out =>
      import Implicits._
      val allDimsIn   = vr.dimensions
      val dimsSorted  = allDimsIn.sortBy(_.size)
      val tooLarge    = dimsSorted.indexWhere(_.size > 8192)
      val dimSplitIdx = if (tooLarge < 0) dimsSorted.size -1 else math.max(0, tooLarge - 1)
      val dimIter     = dimsSorted(dimSplitIdx)
//      val inDims0     = allDimsIn.filter(_ != dimIter)

      def mkCreate(dimIn: nc2.Dimension) = {
        val dimVr   = vr.file.variableMap.get(dimIn.name)
        val dimUnit = dimVr.flatMap(_.units)
        val dimVals = dimVr.fold[ma2.Array] {
          ma2.Array.factory(Array.tabulate(dimIn.size)(_.toDouble))
        } (_.readSafe())
        NetCdfFileUtil.Create(name = dimIn.name, units = dimUnit, values = dimVals)
      }

//      val dimIterVr   = vr.file.variableMap.get(dimIter.name)
//      val dimIterVals = dimIterVr.fold[ma2.Array] {
//        ma2.Array.factory(Array.tabulate(dimIter.size)(_.toDouble))
//      } (_.readSafe())
//
//      val dimIterUnit = dimIterVr.flatMap(_.units)
//      val outDim      = NetCdfFileUtil.Create(name = dimIter.name, units = dimIterUnit, values = dimIterVals)
      val doubleFun   = dt.peer
      val outDims     = allDimsIn.map { d =>
        if (d.name == dimIter.name) mkCreate(d) // outDim
        else NetCdfFileUtil.Keep(d.name)
//        mkCreate(d)
      }
      val inDims = /* ( allDimsIn.map(_.name) */ Vector(dimIter.name) /* inDims0.map(_.name) */
      val proc = NetCdfFileUtil.transform(in = vr.file, out = out, varName = vr.name,
        inDims = inDims,
        outDimsSpec = outDims /* Vector(outDim) */) { case (origin, arr) =>
        val ja: Array[_] = if (arr.isFloat) {
          arr.float1Diterator.map { f =>
            doubleFun(f.toDouble).toFloat
          } .toArray

        } else if (arr.isDouble) {
          arr.double1Diterator.map { d =>
            doubleFun(d)
          } .toArray
        } else {
          sys.error("Variable must have float or double data type")
        }
        ma2.Array.factory(ja: AnyRef)
      }
      import ExecutionContext.Implicits.global
      import processor._
      proc.monitor(printResult = false)
      proc.start()
    }

  private def withSaveFile(title: String)(fun: File => Unit): Unit =
    FileDialog.save(title = title).show(windowOpt).foreach { out0 =>
      val out = out0.replaceExt("nc")
      fun(out)
    }
}
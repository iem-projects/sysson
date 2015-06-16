/*
 *  PlotObjView.scala
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

package at.iem.sysson.gui
package impl

import at.iem.sysson.Plot
import at.iem.sysson.impl.PlotImpl
import de.sciss.desktop
import de.sciss.desktop.OptionPane
import de.sciss.icons.raphael
import de.sciss.lucre.event.Sys
import de.sciss.lucre.matrix.Matrix
import de.sciss.lucre.{matrix, stm}
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.mellite.Workspace
import de.sciss.mellite.gui.ObjView
import de.sciss.mellite.gui.impl.ObjViewImpl
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.Obj

import scala.swing.{Component, Label}

object PlotObjView extends ObjView.Factory {
  type E[S <: Sys[S]] = Plot.Elem[S]
  final val prefix  = "Plot"
  final val icon    = ObjViewImpl.raphaelIcon(raphael.Shapes.LineChart)
  final val typeID  = PlotImpl.ElemImpl.typeID

  def hasDialog: Boolean = true

  private lazy val _init: Unit = ObjView.addFactory(this)

  def init(): Unit = _init

  def apply[S <: SSys[S]](obj: Obj.T[S, E])(implicit tx: S#Tx): ObjView[S] = {
    val name      = obj.name
    val plot      = obj.elem.peer
    val matrixName= plot.matrix.name
    new PlotObjView.Impl(tx.newHandle(obj), name = name, value = new Value(matrixName))
  }

  type Config[S <: SSys[S]] = String

  def initDialog[S <: SSys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                              (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
    val opt = OptionPane.textInput(message = "Enter Plot Name:",
      messageType = OptionPane.Message.Question, initial = "Plot")
    opt.title = "Add Plot"
    val res = opt.show(window)
    res
  }

  def make[S <: SSys[S]](name: String)(implicit tx: S#Tx): List[Obj[S]] = {
    import matrix.Implicits._
    val m0      = Matrix.zeros[S](0)
    val mVar    = Matrix.Var(m0)
    val elem    = Plot.Elem(Plot[S](mVar))
    val obj     = Obj(elem)
    obj.name    = name
    obj :: Nil
  }

  private final class Value(matrixName: String) {
    override def toString = if (matrixName == "zeros[0]") "" else s"matrix: $matrixName"
  }

  private final class Impl[S <: SSys[S]](val obj: stm.Source[S#Tx, Obj.T[S, Plot.Elem]],
                                         var name: String, var value: Value)
    extends ObjViewImpl.Impl[S] with ObjViewImpl.NonEditable[S] {

    def prefix  = PlotObjView.prefix
    def icon    = PlotObjView.icon
    def typeID  = PlotObjView.typeID

    def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean = false   // XXX TODO -- track matrix name

    def isViewable = true

    def openView()(implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
      val frame = PlotFrame(obj())
      Some(frame)
    }

    def configureRenderer(label: Label): Component = {
      val txt    = value.toString
      label.text = txt
      label
    }
  }
}
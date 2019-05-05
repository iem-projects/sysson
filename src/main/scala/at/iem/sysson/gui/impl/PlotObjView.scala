/*
 *  PlotObjView.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2019 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.desktop.OptionPane
import de.sciss.{desktop, equal}
import de.sciss.icons.raphael
import de.sciss.lucre.matrix.Matrix
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.lucre.{matrix, stm}
import de.sciss.mellite.gui.impl.objview.{ListObjViewImpl, ObjViewImpl}
import de.sciss.mellite.gui.{GUI, ListObjView, ObjView}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.Universe
import javax.swing.Icon

import scala.swing.{Component, Label}

object PlotObjView extends ListObjView.Factory {
  type E[S <: Sys[S]]   = Plot[S]
  final val prefix      = "Plot"
  def humanName: String = prefix
  final val icon: Icon  = ObjViewImpl.raphaelIcon(raphael.Shapes.LineChart)
  final val typeId: Int = Plot.typeId
  def category: String  = SwingApplication.categSonification

  def hasMakeDialog: Boolean = true

  private lazy val _init: Unit = ListObjView.addFactory(this)

  def init(): Unit = _init

  def tpe: Obj.Type = Plot

  def mkListView[S <: SSys[S]](obj: E[S])(implicit tx: S#Tx): ListObjView[S] = {
    val plot      = obj // .elem.peer
    val matrixName= plot.matrix.name
    new PlotObjView.Impl(tx.newHandle(obj), value = new Value(matrixName)).initAttrs(obj)
  }

  type Config[S <: Sys[S]] = String

  def initMakeDialog[S <: SSys[S]](window: Option[desktop.Window])(done: MakeResult[S] => Unit)
                                  (implicit universe: Universe[S]): Unit = {
    val pane = OptionPane.textInput(message = "Enter Plot Name:",
      messageType = OptionPane.Message.Question, initial = "Plot")
    pane.title = "Add Plot"
    val res0    = GUI.optionToAborted(pane.show(window))
    val res     = res0
    done(res)
  }

  def initMakeCmdLine[S <: SSys[S]](args: List[String])(implicit universe: Universe[S]): MakeResult[S] = ???

  def canMakeObj: Boolean = true

  def makeObj[S <: SSys[S]](name: String)(implicit tx: S#Tx): List[Obj[S]] = {
    import matrix.Implicits._
    val m0      = Matrix.zeros[S](0)
    val mVar    = Matrix.Var(m0)
    val elem    = Plot[S](mVar)
    val obj     = elem // Obj(elem)
    obj.name    = name
    obj :: Nil
  }

  private final class Value(matrixName: String) {
    override def toString: String = {
      import equal.Implicits._
      if (matrixName === "zeros[0]") "" else s"matrix: $matrixName"
    }
  }

  private final class Impl[S <: SSys[S]](val objH: stm.Source[S#Tx, Plot[S]], var value: Value)
    extends ObjViewImpl.Impl[S]
    with ListObjViewImpl.NonEditable[S]
    with ListObjView[S] {

    def factory: ObjView.Factory  = PlotObjView
    def prefix: String            = PlotObjView.prefix

//    def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean = ...   // track matrix name

    def isViewable = true

    override def obj(implicit tx: S#Tx): Plot[S] = objH()

    def openView(parent: Option[Window[S]])(implicit tx: S#Tx, universe: Universe[S]): Option[Window[S]] = {
      val frame = PlotFrame(obj)
      Some(frame)
    }

    def configureRenderer(label: Label): Component = {
      val txt    = value.toString
      label.text = txt
      label
    }
  }
}
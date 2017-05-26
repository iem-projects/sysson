/*
 *  MatrixObjView.scala
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

package at.iem.sysson.gui
package impl

import javax.swing.Icon

import at.iem.sysson.Vec
import de.sciss.desktop
import de.sciss.icons.raphael
import de.sciss.lucre.matrix.Matrix
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.mellite.gui.impl.{ListObjViewImpl, ObjViewImpl}
import de.sciss.mellite.gui.{ListObjView, ObjView}
import de.sciss.synth.proc.Workspace

import scala.swing.{Component, Label}

object MatrixObjView extends ListObjView.Factory {
  type E[S <: Sys[S]]   = Matrix[S]
  final val prefix      = "Matrix"
  def humanName: String = prefix
  final val icon: Icon  = ObjViewImpl.raphaelIcon(raphael.Shapes.IconView)
  final val typeID: Int = Matrix.typeID
  def category: String  = SwingApplication.categSonification

  def hasMakeDialog: Boolean = false

  private lazy val _init: Unit = ListObjView.addFactory(this)

  def init(): Unit = _init

  def tpe: Obj.Type = Matrix

  def mkListView[S <: SSys[S]](obj: E[S])(implicit tx: S#Tx): ListObjView[S] = {
    val matrixName  = obj.name
    val shape       = obj.shape
    new MatrixObjView.Impl(tx.newHandle(obj), value = new Value(matrixName, shape)).initAttrs(obj)
  }

  type Config[S <: Sys[S]] = Nothing

  def initMakeDialog[S <: SSys[S]](workspace: Workspace[S], window: Option[desktop.Window])(ok: Config[S] => Unit)
                                  (implicit cursor: stm.Cursor[S]): Unit = ()

  def makeObj[S <: SSys[S]](nada: Nothing)(implicit tx: S#Tx): List[Obj[S]] = Nil

  private final class Value(name: String, shape: Vec[Int]) {
    override def toString: String = shape.mkString(s"$name [", "][", "]")
  }

  private final class Impl[S <: SSys[S]](val objH: stm.Source[S#Tx, Matrix[S]], var value: Value)
    extends ObjViewImpl.Impl[S]
      with ListObjViewImpl.NonEditable[S]
      with ListObjView[S] {

    def factory: ObjView.Factory  = MatrixObjView
    def prefix: String            = MatrixObjView.prefix

//    def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean = ...  // track matrix name and shape

    def isViewable = false

    override def obj(implicit tx: S#Tx): Matrix[S] = objH()

    def openView(parent: Option[Window[S]])
                (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = None

    def configureRenderer(label: Label): Component = {
      val txt    = value.toString
      label.text = txt
      label
    }
  }
}
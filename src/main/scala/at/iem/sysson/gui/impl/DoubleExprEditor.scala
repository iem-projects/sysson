/*
 *  DoubleExprEditor.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui
package impl

import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm.Disposable
import de.sciss.swingplus.Spinner
import de.sciss.lucre.synth.expr.{Strings, Doubles}
import de.sciss.lucre.expr.{Expr, Type}
import de.sciss.desktop.UndoManager
import de.sciss.lucre.{expr, stm}
import javax.swing.{JSpinner, SpinnerNumberModel}
import scala.swing.TextComponent
import scala.swing.event.ValueChanged
import at.iem.sysson.gui.edit.{EditExprMap, EditExprVar}
import de.sciss.model.Change
import de.sciss.serial.Serializer

object DoubleExprEditor {
  //  def apply[S <: Sys[S]](expr: Expr[S, Double], name: String, width: Int = 160)
  //                        (implicit tx: S#Tx, cursor: stm.Cursor[S], undoManager: UndoManager): DoubleExprEditor[S]

  def apply[S <: Sys[S], A](map: expr.Map[S, A, Expr[S, Double], Change[Double]], key: A, default: Double,
                            name: String, width: Int = 160)
                        (implicit tx: S#Tx, keySerializer: Serializer[S#Tx, S#Acc, A],
                         cursor: stm.Cursor[S], undoManager: UndoManager): DoubleExprEditor[S] = {
    implicit val valueSer  = Doubles.serializer[S]
    val mapHOpt   = map.modifiableOption.map(tx.newHandle(_))
    val value0    = map.get(key).map(_.value).getOrElse(default)
    val res       = new Impl[S, A](mapHOpt, key = key, value0 = value0, editName = name, maxWidth = width)
    res.observer  = map.changed.react {
      implicit tx => upd => upd.changes.foreach {
        case expr.Map.Added  (`key`, expr)                  => res.update(expr.value)
        // case expr.Map.Removed(`key`, expr)                  => res.update(ha?)
        case expr.Map.Element(`key`, expr, Change(_, now))  => res.update(now       )
        case _ =>
      }
    }

    GUI.fromTx(res.guiInit())
    res
  }

  private final class Impl[S <: Sys[S], A](mapHOpt: Option[stm.Source[S#Tx, expr.Map.Modifiable[S, A, Expr[S, Double], Change[Double]]]],
                                           key: A, value0: Double, editName: String, maxWidth: Int)
                                       (implicit keySerializer: Serializer[S#Tx, S#Acc, A],
                                        cursor: stm.Cursor[S], undoManager: UndoManager)
    extends DoubleExprEditor[S] with ExprEditor[S, Double, Spinner] {

    protected var value = value0

    var observer: Disposable[S#Tx] = _

    protected val tpe: Type[Double] = Doubles

    protected def valueToComponent(): Unit =
      if (sp.value != value) {
        // println("valueToComponent()")
        sp.value = value
      }

    private var sp: Spinner = _

    protected def commit(newValue: Double): Unit = {
      if (value != newValue) {
        mapHOpt.foreach { mapH =>
          val edit = cursor.step { implicit tx =>
            import Doubles.newConst
            implicit val d = Doubles
            EditExprMap[S, A, Double](s"Change $editName", map = mapH(), key = key, value = Some(newConst[S](newValue)))
          }
          undoManager.add(edit)
        }
        value = newValue
      }
      clearDirty()
    }

    protected def createComponent(): Spinner = {
      val spm   = new SpinnerNumberModel(value, Double.MinValue, Double.MaxValue, 0.1)
      sp        = new Spinner(spm)
      val d1    = sp.preferredSize
      d1.width  = math.min(d1.width, maxWidth) // XXX TODO WTF
      sp.preferredSize = d1
      val d2    = sp.maximumSize
      d2.width  = math.min(d2.width, maxWidth)
      sp.maximumSize   = d2
      val d3    = sp.minimumSize
      d3.width  = math.min(d3.width, maxWidth)
      sp.minimumSize = d3
      sp.peer.getEditor match {
        case e: JSpinner.DefaultEditor =>
          val txt = new TextComponent { override lazy val peer = e.getTextField }
          dirty   = Some(DirtyBorder(txt))
          // THIS SHIT JUST DOESN'T WORK, FUCK YOU SWING
          // observeDirty(txt)
        case _ =>
      }
      sp.listenTo(sp)
      sp.reactions += {
        case ValueChanged(_) =>
          sp.value match {
            case v: Double =>
              // println(s"CHANGED $v")
              commit(v)
            case _ =>
          }
      }

      sp
    }
  }
}
trait DoubleExprEditor[S <: Sys[S]] extends Disposable[S#Tx] {
  def component: Spinner
}
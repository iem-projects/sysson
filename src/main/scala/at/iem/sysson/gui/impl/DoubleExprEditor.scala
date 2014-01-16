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
import de.sciss.lucre.synth.expr.Doubles
import de.sciss.lucre.expr.{Expr, Type}
import de.sciss.desktop.UndoManager
import de.sciss.lucre.stm
import javax.swing.{JSpinner, SpinnerNumberModel}
import scala.swing.{TextComponent, Component}

object DoubleExprEditor {
  def apply[S <: Sys[S]](expr: Expr[S, Double], name: String, width: Int = 160)
                        (implicit tx: S#Tx, cursor: stm.Cursor[S], undoManager: UndoManager): DoubleExprEditor[S] = {
    import Doubles._

    val exprH = expr match {
      case Expr.Var(exprV) => Some(tx.newHandle(exprV))
      case _ => None
    }
    val value0 = expr.value
    val res = new Impl[S](exprH, value0 = value0, editName = name, maxWidth = width)
    res.observer = expr.changed.react {
      implicit tx => res.update(_)
    }

    GUI.fromTx(res.guiInit())
    res
  }

  private final class Impl[S <: Sys[S]](val exprH: Option[stm.Source[S#Tx, Expr.Var[S, Double]]], value0: Double,
                                        val editName: String, maxWidth: Int)
                                       (implicit val cursor: stm.Cursor[S], val undoManager: UndoManager)
    extends DoubleExprEditor[S] with ExprEditor[S, Double, Spinner] {

    protected var value = value0

    var observer: Disposable[S#Tx] = _

    protected val tpe: Type[Double] = Doubles

    protected def valueToComponent(): Unit = ???

    protected def createComponent(): Spinner = {
      val spm   = new SpinnerNumberModel(value, Double.MinValue, Double.MaxValue, 0.1)
      val sp    = new Spinner(spm)
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
          observeDirty(txt)
        case _ =>
      }

  //    dirty       = Some(DirtyBorder(txt))
  //
  //    txt.listenTo(txt)
  //    txt.reactions += {
  //      case EditDone(_) => commit(txt.text)
  //    }
  //    observeDirty(txt.peer)
  //    txt
      sp
    }
  }
}
trait DoubleExprEditor[S <: Sys[S]] extends Disposable[S#Tx] {
  def component: Spinner
}
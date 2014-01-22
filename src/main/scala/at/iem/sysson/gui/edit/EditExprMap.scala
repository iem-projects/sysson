/*
 *  EditExprMap.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson.gui.edit

import de.sciss.lucre.{expr, stm, event => evt}
import evt.Sys
import de.sciss.lucre.expr.{Type, Expr}
import javax.swing.undo.{UndoableEdit, AbstractUndoableEdit}
import de.sciss.serial
import de.sciss.model.Change

object EditExprMap {
  def apply[S <: Sys[S], A, B](name: String, map: expr.Map.Modifiable[S, A, Expr[S, B], Change[B]],
                               key: A, value: Option[Expr[S, B]])
                              (implicit tx: S#Tx, cursor: stm.Cursor[S],
                               keySerializer  : serial.Serializer[S#Tx, S#Acc, A],
                               valueType: Type[B]): UndoableEdit = {
    import valueType.{serializer, varSerializer}
    val before = map.get(key)
    val now: Option[Expr[S, B]] = (before, value) match {
      case (Some(Expr.Var(vr)), Some(v)) => return EditExprVar(name, vr, v) // current value is variable
      case (_, None) | (_, Some(Expr.Var(_))) => value  // new value is none or some variable, just put it
      case _ => value.map(valueType.newVar(_))          // new value is some non-variable, wrap it, then put it
    }

    val mapH      = tx.newHandle(map) // (expr.Map.Modifiable.serializer[S, A, Expr[S, B], Change[B]])
    val beforeH   = tx.newHandle(before)
    val nowH      = tx.newHandle(now)
    val res       = new EditMutableMap.Impl(name, key, mapH, beforeH, nowH)
    res.perform()
    res
  }
}

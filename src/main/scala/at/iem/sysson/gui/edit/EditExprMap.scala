/*
 *  EditExprMap.scala
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

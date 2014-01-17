/*
 *  EditMutableMap.scala
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
import evt.{Publisher, Sys}
import javax.swing.undo.{UndoableEdit, AbstractUndoableEdit}
import de.sciss.serial

object EditMutableMap {
  def apply[S <: Sys[S], A, B <: Publisher[S, U], U](name: String, map: expr.Map.Modifiable[S, A, B, U],
                               key: A, value: Option[B])
                              (implicit tx: S#Tx, cursor: stm.Cursor[S],
                               keySerializer  : serial.Serializer[S#Tx, S#Acc, A],
                               valueSerializer: evt.Serializer[S, B]): UndoableEdit = {
    val before = map.get(key)

    val mapH      = tx.newHandle(map) // (expr.Map.Modifiable.serializer[S, A, B, U])
    val beforeH   = tx.newHandle(before)
    val nowH      = tx.newHandle(value)
    val res       = new Impl(name, key, mapH, beforeH, nowH)
    res.perform()
    res
  }

  private[edit] final class Impl[S <: Sys[S], A, B, U](name: String, key: A,
                                              mapH   : stm.Source[S#Tx, expr.Map.Modifiable[S, A, B, U]],
                                              beforeH: stm.Source[S#Tx, Option[B]],
                                              nowH   : stm.Source[S#Tx, Option[B]])(implicit cursor: stm.Cursor[S])
    extends AbstractUndoableEdit {

    override def undo(): Unit = {
      super.undo()
      cursor.step { implicit tx => perform(beforeH) }
    }

    override def redo(): Unit = {
      super.redo()
      cursor.step { implicit tx => perform() }
    }

    private def perform(exprH: stm.Source[S#Tx, Option[B]])(implicit tx: S#Tx): Unit =
      cursor.step { implicit tx =>
        val map = mapH()
        exprH().fold {
          map.remove(key)
        } { expr =>
          map.put(key, expr)
        }
      }

    def perform()(implicit tx: S#Tx): Unit = perform(nowH)

    override def getPresentationName = name
  }
}

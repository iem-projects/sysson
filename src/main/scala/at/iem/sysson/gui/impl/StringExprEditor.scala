/*
 *  StringExprEditor.scala
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

import scala.swing.{Swing, Component, TextField}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.stm
import de.sciss.desktop.UndoManager
import scala.swing.event.EditDone
import javax.swing.border.Border
import de.sciss.icons.raphael
import java.awt.{Insets, Graphics, Color}
import javax.swing.event.{UndoableEditEvent, UndoableEditListener}
import de.sciss.model.Change
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.synth.expr.Strings
import at.iem.sysson.gui.edit.EditExprVar

object StringExprEditor {
  def apply[S <: Sys[S]](expr: Expr[S, String], name: String, columns: Int = 16)
                        (implicit tx: S#Tx, cursor: stm.Cursor[S], undoManager: UndoManager): StringExprEditor[S] = {
    import Strings._
    val exprH     = expr match {
      case Expr.Var(exprV) => Some(tx.newHandle(exprV))
      case _               => None
    }
    val value0    = expr.value
    val res       = new Impl[S](exprH, value0 = value0, name = name)
    res.observer  = expr.changed.react { implicit tx => res.update(_) }

    GUI.fromTx(res.guiInit(columns))
    res
  }

  private final class DirtyBorder(extent: Int, component: Component) extends Border {
    private val icn = raphael.Icon(extent = extent, fill = Color.gray, shadow = raphael.NoPaint)(raphael.Shapes.Pencil)

    val isBorderOpaque = true // false

    private var _visi = false
    def visible = _visi
    def visible_=(value: Boolean): Unit = if (_visi != value) {
      _visi = value
      component.repaint()
    }

    def getBorderInsets(c: java.awt.Component): Insets = new Insets(0, extent + 2, 0, 0) // top left bottom right

    def paintBorder(c: java.awt.Component, g: Graphics, x: Int, y: Int, width: Int, height: Int): Unit = if (_visi)
      icn.paintIcon(c, g, x + 1, y + 1)
  }

  private final class Impl[S <: Sys[S]](exprH: Option[stm.Source[S#Tx, Expr.Var[S, String]]], value0: String,
                                        name: String)
                                       (implicit cursor: stm.Cursor[S], undoMgr: UndoManager)
    extends StringExprEditor[S] with ComponentHolder[TextField] {

    private var value = value0

    var observer: Disposable[S#Tx] = _

    private var dirty: DirtyBorder = _

    def update(ch: Change[String])(implicit tx: S#Tx): Unit =
      if (value != ch.now) GUI.fromTx {
        value           = ch.now
        component.text  = ch.now
        dirty.visible   = false
      }

    def guiInit(columns0: Int): Unit = {
      val txt     = new TextField(value, columns0)
      txt.listenTo(txt)
      val bd0     = txt.border
      val bd      = if (bd0 != null) bd0 else Swing.EmptyBorder
      val txtI    = bd.getBorderInsets(txt.peer)
      dirty       = new DirtyBorder(txt.preferredSize.height - (txtI.top + txtI.bottom + 2), txt)
      txt.border  = Swing.CompoundBorder(outside = bd0, inside = dirty)

      txt.reactions += {
        case EditDone(_) =>
          val newValue = txt.text
          if (value != newValue) {
            exprH.foreach { h =>
              val edit = cursor.step { implicit tx =>
                import Strings._
                EditExprVar[S, String](s"Change $name", expr = h(), value = newConst(newValue))
              }
              undoMgr.add(edit)
            }
            value = newValue
          }
          dirty.visible = false
      }
      txt.peer.getDocument.addUndoableEditListener(new UndoableEditListener {
        def undoableEditHappened(e: UndoableEditEvent): Unit = dirty.visible = true
      })

      component = txt
    }

    def dispose()(implicit tx: S#Tx): Unit = observer.dispose()
  }
}
trait StringExprEditor[S <: Sys[S]] extends Disposable[S#Tx] {
  def component: TextField
}
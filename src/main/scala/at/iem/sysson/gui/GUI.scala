/*
 *  GUI.scala
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

import java.awt.{GraphicsEnvironment, EventQueue}
import scala.swing.{Button, Action, Color, Swing}
import Swing._
import de.sciss.desktop.Window
import de.sciss.lucre.stm.Txn
import scala.concurrent.stm.TxnLocal
import scala.util.control.NonFatal
import de.sciss.icons.raphael
import javax.swing.Icon
import java.awt.geom.Path2D

object GUI {
  def centerOnScreen(w: Window): Unit = placeWindow(w, 0.5f, 0.5f, 0)

  def placeWindow(w: Window, horizontal: Float, vertical: Float, padding: Int): Unit = {
    val ge  = GraphicsEnvironment.getLocalGraphicsEnvironment
    val bs  = ge.getMaximumWindowBounds
    val b   = w.size
    val x   = (horizontal * (bs.width  - padding * 2 - b.width )).toInt + bs.x + padding
    val y   = (vertical   * (bs.height - padding * 2 - b.height)).toInt + bs.y + padding
    w.location = (x, y)
  }

  def requireEDT(): Unit = require(EventQueue.isDispatchThread)

  def defer(thunk: => Unit): Unit =
    if (EventQueue.isDispatchThread) thunk else Swing.onEDT(thunk)

  private val guiCode = TxnLocal(init = Vec.empty[() => Unit], afterCommit = handleGUI)

  private def handleGUI(seq: Vec[() => Unit]): Unit = {
    def exec(): Unit =
      seq.foreach { fun =>
        try {
          fun()
        } catch {
          case NonFatal(e) => e.printStackTrace()
        }
      }

    defer(exec())
  }

  private def iconNormal(fun: Path2D => Unit): Icon =
    raphael.Icon(extent = 20, fill = raphael.TexturePaint(24), shadow = raphael.WhiteShadow)(fun)

  private def iconDisabled(fun: Path2D => Unit): Icon =
    raphael.Icon(extent = 20, fill = new Color(0, 0, 0, 0x7F), shadow = raphael.WhiteShadow)(fun)

  def toolButton(action: Action, iconFun: Path2D => Unit, tooltip: String = ""): Button = {
    val res           = new Button(action)
    res.icon          = iconNormal  (iconFun)
    res.disabledIcon  = iconDisabled(iconFun)
    res.peer.putClientProperty("JButton.buttonType", "textured")
    if (!tooltip.isEmpty) res.tooltip = tooltip
    res
  }

  def fromTx(body: => Unit)(implicit tx: Txn[_]): Unit =
    guiCode.transform(_ :+ (() => body))(tx.peer)
}
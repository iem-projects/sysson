/*
 *  GUI.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import java.awt.{GraphicsEnvironment, EventQueue}
import scala.swing.{Component, Button, Action, Color, Swing}
import Swing._
import de.sciss.desktop
import de.sciss.lucre.stm.Txn
import scala.concurrent.stm.TxnLocal
import scala.util.control.NonFatal
import de.sciss.icons.raphael
import javax.swing.Icon
import java.awt.geom.Path2D
import de.sciss.mellite.gui.ObjView
import at.iem.sysson.gui.impl.DataSourceObjView

object GUI {
  def centerOnScreen(w: desktop.Window): Unit = placeWindow(w, 0.5f, 0.5f, 0)

  def placeWindow(w: desktop.Window, horizontal: Float, vertical: Float, padding: Int): Unit = {
    val ge  = GraphicsEnvironment.getLocalGraphicsEnvironment
    val bs  = ge.getMaximumWindowBounds
    val b   = w.size
    val x   = (horizontal * (bs.width  - padding * 2 - b.width )).toInt + bs.x + padding
    val y   = (vertical   * (bs.height - padding * 2 - b.height)).toInt + bs.y + padding
    w.location = (x, y)
  }

  def fixSize(c: Component): Unit = {
    val d = c.preferredSize
    c.preferredSize = d
    c.minimumSize   = d
    c.maximumSize   = d
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

  private def wordWrap(s: String, margin: Int = 80): String = {
    if (s == null) return "" // fuck java
    val sz = s.length
    if (sz <= margin) return s
    var i = 0
    val sb = new StringBuilder
    while (i < sz) {
      val j = s.lastIndexOf(" ", i + margin)
      val found = j > i
      val k = if (found) j else i + margin
      sb.append(s.substring(i, math.min(sz, k)))
      i = if (found) k + 1 else k
      if (i < sz) sb.append('\n')
    }
    sb.toString()
  }

  def formatException(e: Throwable): String = {
    e.getClass.toString + " :\n" + wordWrap(e.getMessage) + "\n" +
      e.getStackTrace.take(10).map("   at " + _).mkString("\n")
  }

  def windowOption(component: Component): Option[desktop.Window] =
    Option(component.peer.getClientProperty(impl.WindowImpl.WindowKey).asInstanceOf[desktop.Window])

  def registerViews(): Unit = {
    DataSourceObjView
  }
}
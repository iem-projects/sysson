/*
 *  DirtyBorder.scala
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

import scala.swing.{Swing, Component}
import javax.swing.border.Border
import de.sciss.icons.raphael
import java.awt.{Graphics, Insets, Color}

object DirtyBorder {
  def apply(component: Component): DirtyBorder = {
    val bd0     = component.border
    val bd      = if (bd0 != null) bd0 else Swing.EmptyBorder
    val insets  = bd.getBorderInsets(component.peer)
    new DirtyBorder(component.preferredSize.height - (insets.top + insets.bottom + 2), component)
  }
}
final class DirtyBorder(extent: Int, component: Component) extends Border {
  private val icn = raphael.Icon(extent = extent, fill = Color.gray, shadow = raphael.NoPaint)(raphael.Shapes.Pencil)

  component.border = Swing.CompoundBorder(outside = component.border, inside = this)

  val isBorderOpaque = false

  private var _visi = false
  def visible = _visi
  def visible_=(value: Boolean): Unit = if (_visi != value) {
    _visi = value
    component.repaint()
  }

  def getBorderInsets(c: java.awt.Component): Insets = new Insets(0, extent + 2, 0, 0) // top left bottom right

  def paintBorder(c: java.awt.Component, g: Graphics, x: Int, y: Int, width: Int, height: Int): Unit = if (_visi)
    icn.paintIcon(c, g, x + 1, y + 1 + ((height - icn.getIconHeight) >> 1))
}

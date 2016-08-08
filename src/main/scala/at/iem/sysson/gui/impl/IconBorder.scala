/*
 *  IconBorder.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2016 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2016 Hanns Holger Rutz. All rights reserved.
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
import scala.swing.Alignment
import javax.swing.border.Border
import java.awt.{Insets, Graphics, Component}

object IconBorder {
  def apply(icon: Icon, alignment: Alignment.Value = Alignment.Leading, padding: Int = 4): Border = new Border {
    private def orient(component: Component): Alignment.Value = alignment match {
      case Alignment.Leading =>
        val o = component.getComponentOrientation
        if (o.isHorizontal)
          if (o.isLeftToRight) Alignment.Left else Alignment.Right
        else
          Alignment.Top

      case Alignment.Trailing =>
        val o = component.getComponentOrientation
        if (o.isHorizontal)
          if (o.isLeftToRight) Alignment.Right else Alignment.Left
        else
          Alignment.Bottom

      case _ => alignment
    }

    def isBorderOpaque: Boolean = false

    def getBorderInsets(c: Component): Insets = {
      orient(c) match {
        case Alignment.Top    => new Insets(icon.getIconHeight + padding, 0, 0, 0)
        case Alignment.Left   => new Insets(0, icon.getIconWidth  + padding, 0, 0)
        case Alignment.Bottom => new Insets(0, 0, icon.getIconHeight + padding, 0)
        case Alignment.Right  => new Insets(0, 0, 0, icon.getIconWidth  + padding)
        case Alignment.Center => new Insets(0, 0, 0, 0)
      }
    }

    def paintBorder(c: Component, g: Graphics, x: Int, y: Int, width: Int, height: Int): Unit = {
      val (x0, y0) = orient(c) match {
        case Alignment.Top    => ((width -  icon.getIconWidth) >> 1     , 0)
        case Alignment.Left   => (0                                     , (height -  icon.getIconHeight) >> 1)
        case Alignment.Bottom => ((width -  icon.getIconWidth) >> 1     ,  height - (icon.getIconHeight + padding))
        case Alignment.Right  => ( width - (icon.getIconWidth + padding), (height -  icon.getIconHeight) >> 1)
        case Alignment.Center => ((width -  icon.getIconWidth) >> 1     , (height -  icon.getIconHeight) >> 1)
      }
      icon.paintIcon(c, g, x + x0, y + y0)
    }
  }
}
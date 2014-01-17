/*
 *  DropButton.scala
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

import scala.swing.{Component, Button}
import javax.swing.TransferHandler
import javax.swing.TransferHandler.TransferSupport
import java.awt.datatransfer.DataFlavor

object DropButton {
  final val IconSize = 24

  def apply[A](flavor: DataFlavor, tooltip: String)(imp: A => Boolean): Button = {
    val but        = new Button(null: String)
    but.icon       = Icons.Target(IconSize)
    but.focusable  = false
    but.tooltip    = s"Drop $tooltip Here"
    installTransferHandler(but, flavor)(imp)
    but
  }

  def installTransferHandler[A](component: Component, flavor: DataFlavor)(imp: A => Boolean): Unit = {
    component.peer.setTransferHandler(new TransferHandler(null) {
      // how to enforce a drop action: https://weblogs.java.net/blog/shan_man/archive/2006/02/choosing_the_dr.html
      override def canImport(support: TransferSupport): Boolean = component.enabled && {
        // println(support.getDataFlavors.mkString("---supported flavours:---\n ", "\n ", ""))
        // println(s"drop actions: ${support.getSourceDropActions}")

        val res =
          if (support.isDataFlavorSupported(flavor) &&
            ((support.getSourceDropActions & TransferHandler.LINK) != 0)) {
            support.setDropAction(TransferHandler.LINK)
            true
          } else
            false

        // println(s"canImport? $res")
        res
      }
  
      override def importData(support: TransferSupport): Boolean = {
        val t         = support.getTransferable
        val drag      = t.getTransferData(flavor).asInstanceOf[A]
        imp(drag)
      }
    })
  }
}

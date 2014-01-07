/*
 *  MainWindow.scala
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

import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window

object MainWindow {
  val horizontalPlacement   = 1.0f
  val verticalPlacement     = 0.0f
  val placementPadding      = 20
}
class MainWindow extends WindowImpl {
  val view        = MainView()

  def style       = Window.Regular
  def handler     = SwingApplication.windowHandler

  component.peer.getRootPane.putClientProperty("apple.awt.brushMetalLook", true)

  title           = s"${Main.name} v${Main.version}"
  //size          = new Dimension(300, 200)
  contents        = view.component
  resizable       = false
  closeOperation  = Window.CloseIgnore
  reactions += {
    case Window.Closing(_) => SwingApplication.quit()
  }
  pack()

  import MainWindow._
  GUI.placeWindow(this, horizontal = horizontalPlacement, vertical = verticalPlacement, padding = placementPadding)

  front()
}
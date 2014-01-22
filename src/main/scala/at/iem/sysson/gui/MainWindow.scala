/*
 *  MainWindow.scala
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
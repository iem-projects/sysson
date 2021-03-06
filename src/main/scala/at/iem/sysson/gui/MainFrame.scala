/*
 *  MainFrame.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import de.sciss.desktop
import at.iem.sysson.gui.impl.ActionWindowShot
import de.sciss.desktop.WindowHandler

object MainFrame {
  val horizontalPlacement   = 1.0f
  val verticalPlacement     = 0.0f
  val placementPadding      = 20

  def apply(): desktop.Window = {
    instance.front()
    instance
  }

  private lazy val instance = new Impl

  private final class Impl extends desktop.impl.WindowImpl {
    val view        = MainView()

    def handler: WindowHandler = SwingApplication.windowHandler

    addAction("window-shot", new ActionWindowShot(this))
    component.peer.getRootPane.putClientProperty("apple.awt.brushMetalLook", true)

    title           = s"${Main.name} v${Main.version}"
    //size          = new Dimension(300, 200)
    contents        = view.component
    resizable       = false
    closeOperation  = desktop.Window.CloseIgnore
    reactions += {
      case desktop.Window.Closing(_) => SwingApplication.quit()
    }
    pack()

    desktop.Util.placeWindow(this, horizontal = horizontalPlacement, vertical = verticalPlacement, padding = placementPadding)

    // front()
  }
}
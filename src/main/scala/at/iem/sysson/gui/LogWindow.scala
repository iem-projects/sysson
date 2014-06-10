/*
 *  LogWindow.scala
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

import de.sciss.desktop
import de.sciss.desktop.impl.LogWindowImpl
import de.sciss.desktop.{LogPane, WindowHandler}
import java.awt.Font
import de.sciss.mellite.gui.GUI

object LogWindow {
  val horizontalPlacement   = 1.0f
  val verticalPlacement     = 1.0f
  val placementPadding      = 20

  lazy val instance: LogWindow = new LogWindowImpl with LogWindow {
    def handler: WindowHandler = SwingApplication.windowHandler
    log.font = new Font(Font.MONOSPACED, Font.PLAIN, 10)
    GUI.placeWindow(this, horizontalPlacement, verticalPlacement, placementPadding)
  }
}
trait LogWindow extends desktop.Window {
  def log: LogPane
}
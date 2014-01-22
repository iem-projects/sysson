/*
 *  LogWindow.scala
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

import de.sciss.scalainterpreter.LogPane
import impl.{LogWindowImpl => Impl}
import de.sciss.desktop

object LogWindow {
  val horizontalPlacement   = 1.0f
  val verticalPlacement     = 1.0f
  val placementPadding      = 20

  lazy val instance: LogWindow  = new Impl
}
abstract class LogWindow extends desktop.Window {
  def log: LogPane
}
package at.iem.sysson
package gui

import de.sciss.scalainterpreter.LogPane
import impl.{LogWindowImpl => Impl}
import de.sciss.desktop.Window

object LogWindow {
  val horizontalPlacement   = 1.0f
  val verticalPlacement     = 1.0f
  val placementPadding      = 20

  lazy val instance: LogWindow  = new Impl
}
abstract class LogWindow extends Window {
  def log: LogPane
}
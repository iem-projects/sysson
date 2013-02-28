package at.iem.sysson
package gui

import swing.Frame
import de.sciss.scalainterpreter.LogPane
import impl.{LogWindowImpl => Impl}

object LogWindow {
  val horizontalPlacement   = 1.0f
  val verticalPlacement     = 1.0f
  val placementPadding      = 20

  lazy val instance: LogWindow  = new Impl
}
abstract class LogWindow extends Frame {
  def log: LogPane
}
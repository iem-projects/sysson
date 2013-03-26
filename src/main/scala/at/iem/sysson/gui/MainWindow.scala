package at.iem.sysson
package gui

import swing.{Swing, Frame}
import java.awt.{GraphicsEnvironment, Dimension}
import javax.swing.WindowConstants
import Swing._
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window

object MainWindow {
  val horizontalPlacement   = 1.0f
  val verticalPlacement     = 0.0f
  val placementPadding      = 20
}
class MainWindow extends WindowImpl {
  val view  = MainView()

  def style   = Window.Regular
  def handler = SwingApplication.windowHandler

  title     = s"${Main.name} v${Main.version}"
  //size      = new Dimension(300, 200)
  contents  = view.component
  resizable = false
  closeOperation  = Window.CloseIgnore
  reactions += {
    case Window.Closing(_) => SwingApplication.quit()
  }
  pack()

  import MainWindow._
  GUI.placeWindow(this, horizontal = horizontalPlacement, vertical = verticalPlacement, padding = placementPadding)

  front()
}
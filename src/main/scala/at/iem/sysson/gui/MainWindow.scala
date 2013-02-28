package at.iem.sysson
package gui

import swing.{Swing, Frame}
import java.awt.{GraphicsEnvironment, Dimension}
import javax.swing.WindowConstants
import Swing._

object MainWindow {
  val horizontalPlacement   = 1.0f
  val verticalPlacement     = 0.0f
  val placementPadding      = 20
}
class MainWindow extends Frame {
  val view  = MainView()

  menuBar   = MenuFactory.root.create(this)
  title     = s"${Main.name} v${Main.version}"
  //size      = new Dimension(300, 200)
  contents  = view.component
  resizable = false
  peer.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  pack()

  import MainWindow._
  GUI.placeWindow(this, horizontal = horizontalPlacement, vertical = verticalPlacement, padding = placementPadding)

  open()
}
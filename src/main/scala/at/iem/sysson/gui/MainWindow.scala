package at.iem.sysson
package gui

import swing.{Swing, Frame}
import java.awt.{GraphicsEnvironment, Dimension}
import javax.swing.WindowConstants
import Swing._

object MainWindow {
  val horizontalPlacement   = 1.0
  val verticalPlacement     = 0.0
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

  location = {
    import MainWindow._
    val ge  = GraphicsEnvironment.getLocalGraphicsEnvironment
    val bs  = ge.getMaximumWindowBounds
    val b   = size
    val x   = (horizontalPlacement * (bs.width  - placementPadding * 2 - b.width)).toInt + bs.x + placementPadding
    val y   = (verticalPlacement   * (bs.height - placementPadding * 2 - b.height)).toInt + bs.y + placementPadding
    (x, y)
  }

  open()
}
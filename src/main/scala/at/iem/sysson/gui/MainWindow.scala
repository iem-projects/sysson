package at.iem.sysson
package gui

import swing.Frame
import java.awt.Dimension
import javax.swing.WindowConstants

class MainWindow extends Frame {
  title     = s"${Main.name} v${Main.version}"
  size      = new Dimension(300, 200)
  resizable = false
  peer.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  centerOnScreen()
  open()
}
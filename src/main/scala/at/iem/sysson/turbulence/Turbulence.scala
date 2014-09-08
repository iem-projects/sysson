package at.iem.sysson
package turbulence

import java.awt.EventQueue

import scala.swing.Frame

object Turbulence extends App with Runnable {
  EventQueue.invokeLater(this)

  def run(): Unit = {
    Main.run()
    new Frame {
      contents = new DymaxionView
      pack()
      centerOnScreen()
      open()
    }
  }
}

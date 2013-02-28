package at.iem.sysson
package gui

import swing.{Component, ScrollPane, Swing, Frame}
import de.sciss.scalainterpreter.LogPane
import java.io.OutputStream
import javax.swing.BorderFactory

object LogWindow {
  val horizontalPlacement   = 1.0f
  val verticalPlacement     = 1.0f
  val placementPadding      = 20
}
// lazy window - opens as soon as something goes to the console
class LogWindow extends Frame {
  frame =>
  peer.getRootPane.putClientProperty("Window.style", "small")

  private val p = LogPane()

  private val observer: OutputStream = new OutputStream {
    override def write(b: Array[Byte], off: Int, len: Int) {
      p.makeDefault()               // detaches this observer
      p.outputStream.write(b, off, len)
      Swing.onEDT(frame.open())     // there we go
    }

    def write(b: Int) {
      write(Array(b.toByte), 0, 1)
    }
  }

  Console.setOut(observer)
  Console.setErr(observer)

  contents = new ScrollPane {
    contents  = Component.wrap(p.component)
    border    = BorderFactory.createEmptyBorder()
  }

  title = "Log"
  pack()
  import LogWindow._
  GUI.placeWindow(frame, horizontal = horizontalPlacement, vertical = verticalPlacement, padding = placementPadding)
}
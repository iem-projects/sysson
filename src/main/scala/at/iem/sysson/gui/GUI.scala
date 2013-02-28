package at.iem.sysson
package gui

import java.awt.{GraphicsEnvironment, Toolkit, EventQueue}
import swing.{Window, Swing}
import javax.swing.KeyStroke
import java.awt.event.InputEvent
import Swing._

object GUI {
  private var isInitialized = false

  def stroke(code: Int, modifiers: Int): KeyStroke = KeyStroke.getKeyStroke(code, modifiers)
  lazy val meta  = Toolkit.getDefaultToolkit.getMenuShortcutKeyMask
  lazy val shift = InputEvent.SHIFT_MASK

  def placeWindow(w: Window, horizontal: Float, vertical: Float, padding: Int) {
    val ge  = GraphicsEnvironment.getLocalGraphicsEnvironment
    val bs  = ge.getMaximumWindowBounds
    val b   = w.size
    val x   = (horizontal * (bs.width  - padding * 2 - b.width )).toInt + bs.x + padding
    val y   = (vertical   * (bs.height - padding * 2 - b.height)).toInt + bs.y + padding
    w.location = (x, y)
  }

  def init() {
    requireEDT()
    require(!isInitialized)

    val dh = DocumentHandler.instance
    dh.addListener {
      case DocumentHandler.Opened(doc) => Swing.onEDT(mkDocView(doc))
    }
    dh.allDocuments.foreach(mkDocView)

    new MainWindow
    LogWindow.instance  // initializes it

    isInitialized = true
  }

  def requireEDT() { require(EventQueue.isDispatchThread) }

  def defer(thunk: => Unit) {
    if (EventQueue.isDispatchThread) thunk else Swing.onEDT(thunk)
  }

  private def mkDocView(doc: Document): DocumentView = {
    impl.DocumentViewHandlerImpl.mkView(doc)
  }
}
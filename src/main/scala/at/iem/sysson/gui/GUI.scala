package at.iem.sysson
package gui

import java.awt.{Toolkit, EventQueue}
import swing.Swing
import javax.swing.KeyStroke
import java.awt.event.InputEvent

object GUI {
  private var isInitialized = false

  def stroke(code: Int, modifiers: Int): KeyStroke = KeyStroke.getKeyStroke(code, modifiers)
  lazy val meta  = Toolkit.getDefaultToolkit.getMenuShortcutKeyMask
  lazy val shift = InputEvent.SHIFT_MASK

  def init() {
    requireEDT()
    require(!isInitialized)

    val dh = DocumentHandler.instance
    dh.addListener {
      case DocumentHandler.Opened(doc) => Swing.onEDT(mkDocView(doc))
    }
    dh.allDocuments.foreach(mkDocView)

    new MainWindow

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
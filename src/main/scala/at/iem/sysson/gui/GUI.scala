package at.iem.sysson
package gui

import java.awt.EventQueue
import swing.Swing

object GUI {
  def init() {
    require(EventQueue.isDispatchThread)

    DocumentHandler.instance.addListener {
      case DocumentHandler.Opened(doc) => Swing.onEDT(openDoc(doc))
    }

    new MainWindow
  }

  private def openDoc(doc: Document) {
    DocumentView(doc)
  }
}
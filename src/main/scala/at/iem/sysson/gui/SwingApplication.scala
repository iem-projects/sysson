package at.iem.sysson
package gui

import de.sciss.desktop.impl.SwingApplicationImpl
import at.iem.sysson
import de.sciss.desktop.Menu
import swing.Swing

object SwingApplication extends SwingApplicationImpl("SysSon") {
  type Document = sysson.Document

  def quit() {
    sys.exit()
  }

  override def init() {
    val dh = DocumentHandler.instance
    dh.addListener {
      case DocumentHandler.Opened(doc) => Swing.onEDT(mkDocView(doc))
    }
    dh.allDocuments.foreach(mkDocView)

    LogWindow.instance          // initializes it
    System.setErr(Console.err)  // por que?
    new MainWindow
  }

  private def mkDocView(doc: Document): DocumentView = {
    impl.DocumentViewHandlerImpl.mkView(doc)
  }

  protected def menuFactory: Menu.Root = MenuFactory.root
}
/*
 *  SwingApplication.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import de.sciss.desktop.impl.{WindowHandlerImpl, SwingApplicationImpl}
import de.sciss.desktop.{WindowHandler, Menu}
import de.sciss.lucre.event.Sys
import language.existentials
import javax.swing.UIManager
import scala.util.control.NonFatal

/** The main entry point for the desktop Swing application.
  * Please note that this should _not_ be the main class of the project,
  * but you should always invoke `at.iem.sysson.Main`, because it first
  * initializes some type extensions that would be missing otherwise.
  */
object  SwingApplication extends SwingApplicationImpl("SysSon") {
  type Document = DocumentHandler.Document  // sysson.DataSourceLike

  override lazy val windowHandler: WindowHandler = new WindowHandlerImpl(this, menuFactory) {
    override lazy val usesInternalFrames = {
      false // XXX TODO: eventually a preferences entry
    }

    override def usesNativeDecoration: Boolean = Prefs.nativeWindowDecoration.getOrElse(true)
  }

  override def init(): Unit = {
    try {
      // WebLookAndFeel.install()
      UIManager.installLookAndFeel("Web Look And Feel", "com.alee.laf.WebLookAndFeel" ) // classOf[WebLookAndFeel].getName
      UIManager.setLookAndFeel(Prefs.lookAndFeel.getOrElse(Prefs.defaultLookAndFeel).getClassName)
    } catch {
      case NonFatal(_) =>
    }

    // work-around for WebLookAndFeel bug #118
    new javax.swing.JSpinner

    val dh = DocumentHandler.instance // initialize
    DocumentViewHandler.instance      // initialize

    dh.addListener {
      case DocumentHandler.Opened(doc) => mkDocView(doc)
    }
    dh.allDocuments.foreach(mkDocView)

    // keep using IntelliJ console when debugging
    if (!sys.props.getOrElse("sun.java.command", "?").contains("intellij")) {
      LogWindow.instance // initializes it
      Console.setOut(System.out) // stupidly deprecated, but needed, because Console is broken
      Console.setErr(System.err) // stupidly deprecated, but needed, because Console is broken
    }

    MainWindow()
  }

  // cf. http://stackoverflow.com/questions/20982681/existential-type-or-type-parameter-bound-failure
  private def mkDocView(doc: Workspace[_]): Unit =
    mkDocView1(doc.asInstanceOf[Workspace[S] forSome { type S <: Sys[S] }])

  private def mkDocView1[S <: Sys[S]](doc: Workspace[S]): Unit =
    doc.cursor.step { implicit tx =>
      impl.DocumentViewHandlerImpl.mkWindow(doc)
    }

  protected def menuFactory: Menu.Root = MenuFactory.root
}
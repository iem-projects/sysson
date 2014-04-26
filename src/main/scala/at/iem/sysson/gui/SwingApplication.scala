/*
 *  SwingApplication.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import de.sciss.desktop.impl.{WindowHandlerImpl, SwingApplicationImpl}
import de.sciss.desktop.{WindowHandler, Desktop, Menu}
import de.sciss.lucre.event.Sys
import language.existentials
import javax.swing.UIManager
import scala.util.control.NonFatal

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

    val dh = DocumentHandler.instance // initialize
    DocumentViewHandler.instance      // initialize

    dh.addListener {
      case DocumentHandler.Opened(doc) => mkDocView(doc)
    }
    dh.allDocuments.foreach(mkDocView)

    // keep using IntelliJ console when debugging
    if (!sys.props.getOrElse("sun.java.command", "?").contains("intellij")) {
      LogWindow.instance          // initializes it
      System.setErr(Console.err)  // por que?
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
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

import de.sciss.desktop.impl.SwingApplicationImpl
import de.sciss.desktop.Menu
import de.sciss.lucre.event.Sys
import language.existentials

object SwingApplication extends SwingApplicationImpl("SysSon") {
  type Document = DocumentHandler.Document  // sysson.DataSourceLike

  override def init(): Unit = {
    val dh = DocumentHandler.instance
    dh.addListener {
      case DocumentHandler.Opened(doc) => mkDocView(doc)
    }
    dh.allDocuments.foreach(mkDocView)

    // keep using IntelliJ console when debugging
    if (!sys.props.getOrElse("sun.java.command", "?").contains("intellij")) {
      LogWindow.instance          // initializes it
      System.setErr(Console.err)  // por que?
    }
    new MainWindow
  }

  // cf. http://stackoverflow.com/questions/20982681/existential-type-or-type-parameter-bound-failure
  private def mkDocView(doc: Workspace[_]): Unit =
    mkDocView1(doc.asInstanceOf[Workspace[S] forSome { type S <: Sys[S] }])

  private def mkDocView1[S <: Sys[S]](doc: Workspace[S]): Unit =
    doc.cursor.step { implicit tx =>
      impl.DocumentViewHandlerImpl.mkView(doc)
    }

  protected def menuFactory: Menu.Root = MenuFactory.root
}
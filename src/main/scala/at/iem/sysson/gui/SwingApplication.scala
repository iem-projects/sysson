/*
 *  SwingApplication.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
    dh.allDocuments.foreach(doc => mkDocView(doc))

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

  private def mkDocView1[S <: Sys[S]](doc: Workspace[S]): Unit = {
    doc.cursor.step { implicit tx =>
      impl.DocumentViewHandlerImpl.mkView(doc)
    }
  }

  protected def menuFactory: Menu.Root = MenuFactory.root
}
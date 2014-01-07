/*
 *  SwingApplication.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013 Institute of Electronic Music and Acoustics, Graz.
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
import at.iem.sysson
import de.sciss.desktop.Menu
import swing.Swing

object SwingApplication extends SwingApplicationImpl("SysSon") {
  type Document = sysson.DataSourceLike

  override def init(): Unit = {
    val dh = DocumentHandler.instance
    dh.addListener {
      case DocumentHandler.Opened(doc) => Swing.onEDT(mkDocView(doc))
    }
    dh.allDocuments.foreach(mkDocView)

    // keep using IntelliJ console when debugging
    if (!sys.props.getOrElse("sun.java.command", "?").contains("intellij")) {
      LogWindow.instance          // initializes it
      System.setErr(Console.err)  // por que?
    }
    new MainWindow
  }

  private def mkDocView(doc: DataSourceLike): DocumentView = {
    impl.DocumentViewHandlerImpl.mkView(doc)
  }

  protected def menuFactory: Menu.Root = MenuFactory.root
}
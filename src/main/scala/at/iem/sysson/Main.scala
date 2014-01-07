/*
 *  Main.scala
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

import gui.SwingApplication
import scala.util.control.NonFatal

object Main extends App with Runnable {
  final val useNcView = false
  final val useGUI    = true

  run()

  lazy val name   : String = buildInfoString("name"   )
  lazy val version: String = buildInfoString("version")

  private def buildInfoString(key: String): String = try {
    val clazz = Class.forName("at.iem.sysson.BuildInfo")
    val m     = clazz.getMethod(key)
    m.invoke(null).toString
  } catch {
    case NonFatal(e) => "???"
  }

  def run(): Unit = {
    logInfo(s"Welcome to $name v$version")
    if (useNcView) {
      val ncView = NcviewSync()
      ncView.dump(on = true)
      ncView.start()
    }

    if (useGUI) {
      SwingApplication.main(Array.empty)
      //      // this is just for simple IDEA run configurations.
      //      // the app-bundle will have these already
      //      sys.props("com.apple.mrj.application.apple.menu.about.name")  = name
      //      sys.props("apple.laf.useScreenMenuBar")                       = "true"
      //      Swing.onEDT(gui.GUI.init())
    }
  }
}
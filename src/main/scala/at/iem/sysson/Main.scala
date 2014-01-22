/*
 *  Main.scala
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

import gui.SwingApplication
import scala.util.control.NonFatal
import at.iem.sysson.legacy.NcviewSync

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
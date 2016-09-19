/*
 *  Main.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2016 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson

import at.iem.sysson
import at.iem.sysson.gui.SwingApplication

import scala.util.control.NonFatal

object Main {
  lazy val name   : String = buildInfoString("name"   )
  lazy val version: String = buildInfoString("version")

  private def buildInfoString(key: String): String = try {
    val clazz = Class.forName("at.iem.sysson.BuildInfo")
    val m     = clazz.getMethod(key)
    m.invoke(null).toString
  } catch {
    case NonFatal(e) => "?"
  }

  def main(args: Array[String]): Unit = {
    logInfo(s"Welcome to $name v$version")

    SwingApplication.main(args)
  }
}
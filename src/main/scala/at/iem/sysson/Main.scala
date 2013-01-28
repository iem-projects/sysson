package at.iem.sysson

import util.control.NonFatal
import swing.Swing

object Main extends App with Runnable {
  final val useGUI = true

  run()

  lazy val name: String    = buildInfoString("name")
  lazy val version: String = buildInfoString("version")

  private def buildInfoString(key: String): String = try {
    val clazz = Class.forName("at.iem.sysson.BuildInfo")
    val m     = clazz.getMethod(key)
    m.invoke(null).toString
  } catch {
    case NonFatal(e) => "???"
  }

  def run() {
    logInfo(s"Welcome to ${name} v${version}")
    val ncview = NcviewSync()
    ncview.dump(on = true)
    ncview.start()

    if (useGUI) Swing.onEDT(gui.GUI.init())
  }
}
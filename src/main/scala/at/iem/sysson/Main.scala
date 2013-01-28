package at.iem.sysson

import util.control.NonFatal

object Main extends App with Runnable {
  run()

  def version: String = try {
    val clazz = Class.forName("at.iem.sysson.BuildInfo")
    val m     = clazz.getMethod("version")
    m.invoke(null).toString
  } catch {
    case NonFatal(e) => "???"
  }

  def run() {
    logInfo("Welcome to SysSon v" + version)
    val ncview = NcviewSync()
    ncview.dump(on = true)
    ncview.start()
  }
}
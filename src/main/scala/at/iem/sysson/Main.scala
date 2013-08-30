package at.iem.sysson

import gui.SwingApplication
import scala.util.control.NonFatal

object Main extends App with Runnable {
  final val useGUI = true

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
    val ncview = NcviewSync()
    ncview.dump(on = true)
    ncview.start()

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
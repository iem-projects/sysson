package at.iem.sysson

object SimpleTest extends App {
  Main.main(new Array[String](0))

  DocumentHandler.instance.openRead(defaultPath)
}
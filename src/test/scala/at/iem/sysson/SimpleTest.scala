package at.iem.sysson

import Implicits._

object SimpleTest extends App {
  Main.main(new Array[String](0))

  DocumentHandler.instance.openRead(defaultFile.path)
}
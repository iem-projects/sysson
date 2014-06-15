package at.iem.sysson.graph

import de.sciss.synth.{ScalarRated, UGenInLike, GE}

case class LoadBuffer(key: String) extends GE.Lazy with ScalarRated {
  protected def makeUGens: UGenInLike = ???
}
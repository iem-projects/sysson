package at.iem.sysson.fscape.graph

import de.sciss.fscape.{GE, UGenGraph, UGenInLike}

final case class IsFill(variable: Matrix, in: GE) extends GE.Lazy {
  protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = ???
}
package at.iem.sysson
package graph
package impl

import de.sciss.synth.{UGenGraph, UGenInLike, GE}
import at.iem.sysson.sound.UGenGraphBuilder

trait LazyImpl extends GE.Lazy {
  protected final def makeUGens: UGenInLike =
    UGenGraph.builder match {
      case b: UGenGraphBuilder => makeUGens(b)
      case _ => sys.error("Expansion out of context")
    }

  protected def makeUGens(builder: UGenGraphBuilder): UGenInLike
}

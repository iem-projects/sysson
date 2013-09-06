package at.iem.sysson
package graph
package impl

import de.sciss.synth.{ScalarRated, GE, UGenInLike}
import at.iem.sysson.sound.UGenGraphBuilder

object UserValueImpl {
  def value(peer: UserValue): GE = new ValueImpl(peer)

  private final case class ValueImpl(peer: UserValue) extends LazyImpl with ScalarRated {
    protected def makeUGens(b: UGenGraphBuilder): UGenInLike =
      b.addScalarUserValue(peer)
  }
}

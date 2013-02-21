package at.iem.sysson
package sound

import de.sciss.synth._
import impl.{SonficationImpl => Impl}
import concurrent.duration.Duration

object Sonification {
  val emptyGraph = SynthGraph(Vector.empty, Set.empty)

  def apply(name: String) = new Sonification(name)

  case class MissingInput(key: String) extends RuntimeException(s"Missing sonification input for key '$key'")
}
class Sonification(var name: String) {
  var graph:    SynthGraph                      = Sonification.emptyGraph
  var matrices: Map[String, MatrixSpec]         = Map.empty
  var mapping:  Map[String, SonificationSource] = Map.empty

  def play    (rate:     Double  ): Synth = Impl.play    (this, rate    )
  def playOver(duration: Duration): Synth = Impl.playOver(this, duration)

  override def toString = s"Sonification($name)${hashCode().toHexString}"
}
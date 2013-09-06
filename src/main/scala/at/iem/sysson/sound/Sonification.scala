package at.iem.sysson
package sound

import de.sciss.synth._
import impl.{SonificationImpl => Impl}
import scala.concurrent.{ExecutionContext, Future}

object Sonification {
  // val emptyGraph = SynthGraph(Vector.empty, Set.empty)

  def apply(name: String): Sonification = Impl(name)

  case class MissingInput(key: String) extends RuntimeException(s"Missing sonification input for key '$key'")

  final val DefaultVariable = "<default>"

  trait Prepared {
    def play(): Synth
  }
}
trait Sonification {
  /** A user chosen name associated with the sonification. */
  var name: String

  /** The patch describing the audio signal processing of the sonification. */
  var patch: Patch

  /** OBSOLETE */
  var mapping:  Map[String, SonificationSource]

  /** Prepares the data to be sonified.
    *
    * @return a future of the prepared sonification, ready to play
    */
  def prepare()(implicit context: ExecutionContext): Future[Sonification.Prepared]

  /** Maps between logical names and variable sections used as data source. */
  var variableMap: Map[String, VariableSection]

  /** Maps between keys and user provided values. */
  var userValueMap: Map[String, Double]

  /** Debugging method that writes the sonification's synth definition to a file on the user desktop. */
  def _debug_writeDef(): Unit
}
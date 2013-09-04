package at.iem.sysson
package sound

import de.sciss.synth._
import impl.{SonificationImpl => Impl}
import concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}

object Sonification {
  val emptyGraph = SynthGraph(Vector.empty, Set.empty)

  def apply(name: String): Sonification = Impl(name)

  case class MissingInput(key: String) extends RuntimeException(s"Missing sonification input for key '$key'")

  final val DefaultVariable = "<default>"

  trait Prepared {
    def play(): Synth
  }
}
trait Sonification {
  var name:     String
//  var graph:    SynthGraph
  def graph:     () => GE
  def graph_=(body: => GE): Unit
  // var matrices: Map[String, MatrixSpec]
  var mapping:  Map[String, SonificationSource]

  //  def play    (rate:     Double  ): Synth
  //  def playOver(duration: Duration): Synth

  def prepare()(implicit context: ExecutionContext): Future[Sonification.Prepared]

  // var selections: Map[String, (Int, Int)]

  var variableMap: Map[String, VariableSection]

  // var dimensionMap: Map[String, VariableSection]

  def _debug_writeDef(): Unit
}
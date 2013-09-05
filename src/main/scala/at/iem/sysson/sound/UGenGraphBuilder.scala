package at.iem.sysson
package sound

import de.sciss.synth.{SynthGraph, GE, UGenGraph}
import at.iem.sysson.graph.SelectedLike
import impl.{UGenGraphBuilderImpl => Impl}

object UGenGraphBuilder {
  /** XXX TODO: need to accommodate reductions such as averaging
    *
    * @param control   the control name to use
    * @param peer      the client side variable section
    * @param stream    the index of the dimension to stream, of `-1` if using static buffer
    */
  case class Section(control: String, peer: VariableSection, stream: Int) {
    def variable    = peer.variable
    def isStreaming = stream >= 0
  }

  case class Result(graph: UGenGraph, sections: Vec[Section])

  def apply(sonif: Sonification, sg: SynthGraph): Result = Impl(sonif, sg)
}
trait UGenGraphBuilder extends UGenGraph.Builder {
  def addMatrixIn(m: MatrixIn): GE
  def getMatrixInSource(m: MatrixIn): SonificationSource

  // ---- TODO ----

  def addScalarSelection(range: SelectedLike): GE
}
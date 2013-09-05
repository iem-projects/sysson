package at.iem.sysson
package sound

import de.sciss.synth.{SynthGraph, GE, UGenGraph}
import at.iem.sysson.graph.{Var, SelectedLike}
import impl.{UGenGraphBuilderImpl => Impl}

object UGenGraphBuilder {
  /** XXX TODO: need to accommodate reductions such as averaging
    *
    * @param controlName  the control name to use
    * @param peer         the client side variable section
    * @param streamDim    the index of the dimension to stream, of `-1` if using static buffer
    * @param streamID     send-trig identifier for streamed sections
    */
  case class Section(controlName: String, peer: VariableSection, streamDim: Int, streamID: Int) {
    def variable    = peer.variable
    def isStreaming = streamDim >= 0
  }

  case class Result(graph: UGenGraph, sections: Vec[Section])

  def apply(sonif: Sonification, sg: SynthGraph): Result = Impl(sonif, sg)
}
trait UGenGraphBuilder extends UGenGraph.Builder {
  def addMatrixIn(m: MatrixIn): GE
  def getMatrixInSource(m: MatrixIn): SonificationSource

  def addScalarSelection(range: SelectedLike): GE
  def addAudioSelection (range: SelectedLike, freq: GE): GE
  def addAudioVariable  (variable: Var.Playing): GE
}
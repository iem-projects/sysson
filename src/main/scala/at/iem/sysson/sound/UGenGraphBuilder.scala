package at.iem.sysson
package sound

import de.sciss.synth.{SynthGraph, GE, UGenGraph}
import at.iem.sysson.graph.SelectedLike
import impl.{UGenGraphBuilderImpl => Impl}

object UGenGraphBuilder {
  case class Result(graph: UGenGraph, sections: Vec[(String, VariableSection)])

  def apply(sonif: Sonification, sg: SynthGraph): Result = Impl(sonif, sg)
}
trait UGenGraphBuilder extends UGenGraph.Builder {
  def addMatrixIn(m: MatrixIn): GE
  def getMatrixInSource(m: MatrixIn): SonificationSource

  // ---- TODO ----

  def addScalarSelection(range: SelectedLike): GE
}
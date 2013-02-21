package at.iem.sysson
package sound

import de.sciss.synth.{GE, UGenGraph}

private[sound] trait UGenGraphBuilder extends UGenGraph.Builder {
  def addMatrixIn(m: MatrixIn): GE
}
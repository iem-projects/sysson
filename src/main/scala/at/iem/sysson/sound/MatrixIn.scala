package at.iem.sysson
package sound

import de.sciss.synth._

object MatrixIn {
  def ar(key: String): MatrixIn = apply(audio, key)
  def kr(key: String): MatrixIn = apply(control, key)

  private def outsideOfContext() = sys.error( "Expansion out of context" )
}
case class MatrixIn(rate: Rate, key: String) extends GE.Lazy {
  protected def makeUGens: UGenInLike = {
    UGenGraph.builder match {
      case b: UGenGraphBuilder  => b.addMatrixIn(this)
      case _                    => MatrixIn.outsideOfContext()
    }
  }
}
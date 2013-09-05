package at.iem.sysson
package sound

import de.sciss.synth._

object MatrixIn {
  def ar(key: String): MatrixIn = apply(audio, key)
  def kr(key: String): MatrixIn = apply(control, key)

  private def outsideOfContext() = sys.error( "Expansion out of context" )

  private final case class NumRows(m: MatrixIn) extends GE {
    def rate = scalar
    def expand: UGenInLike = builder.getMatrixInSource(m).numRows
  }

  private final case class NumColumns(m: MatrixIn) extends GE {
    def rate = scalar
    def expand: UGenInLike = builder.getMatrixInSource(m).numColumns
  }

  private def builder: UGenGraphBuilder = UGenGraph.builder match {
    case b: UGenGraphBuilder  => b
    case _                    => MatrixIn.outsideOfContext()
  }
}
case class MatrixIn(rate: Rate, key: String) extends GE.Lazy {
  import MatrixIn._

  protected def makeUGens: UGenInLike = builder.addMatrixIn(this)

  def numRows:    GE = NumRows   (this)
  def numColumns: GE = NumColumns(this)
}
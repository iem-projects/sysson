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
        case b: UGenGraphBuilder =>
          ???
//          val numChannels = b.addScanIn( key )
//           val ctlName = inControlName( key )
//           if( numChannels == 1 ) {
//              ctlName.ar( default ).expand
//           } else if( numChannels > 1 ) {
//              ctlName.ar( default, IIdxSeq.fill( numChannels - 1 )( default ): _* ).expand
//           } else {
//              UGenInGroup.empty
//           }

        case other => MatrixIn.outsideOfContext()
     }
  }
}
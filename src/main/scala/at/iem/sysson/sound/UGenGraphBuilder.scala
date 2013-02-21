package at.iem.sysson
package sound

import de.sciss.synth._
import util.control.ControlThrowable

//private[sound] object UGenGraphBuilder {
//  final case class MissingIn(key: String) extends ControlThrowable
//}
private[sound] trait UGenGraphBuilder extends UGenGraph.Builder {
  def addMatrixIn(key: String): Int
}
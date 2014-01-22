/*
 *  SonificationOLD.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package sound

import de.sciss.synth._
import impl.{SonificationOLDImpl => Impl}
import scala.concurrent.{ExecutionContext, Future}

object SonificationOLD {
  // val emptyGraph = SynthGraph(Vector.empty, Set.empty)

  def apply(name: String): SonificationOLD = Impl(name)

  case class MissingInput(key: String) extends RuntimeException(s"Missing sonification input for key '$key'")

  final val DefaultVariable = "<default>"

  trait Prepared {
    def play(): Synth
  }
}
trait SonificationOLD {
  /** A user chosen name associated with the sonification. */
  var name: String

  /** The patch describing the audio signal processing of the sonification. */
  var patch: PatchOLD

  /** OBSOLETE */
  var mapping:  Map[String, SonificationSourceOLD]

  /** Prepares the data to be sonified.
    *
    * @return a future of the prepared sonification, ready to play
    */
  def prepare()(implicit context: ExecutionContext): Future[SonificationOLD.Prepared]

  /** Maps between logical names and variable sections used as data source. */
  var variableMap: Map[String, VariableSection]

  /** Maps between keys and user provided values. */
  var userValueMap: Map[String, Double]

  /** Debugging method that writes the sonification synth definition to a file on the user desktop. */
  def _debug_writeDef(): Unit
}
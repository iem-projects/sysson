/*
 *  Sonification.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package sound

import de.sciss.synth._
import impl.{SonificationImpl => Impl}
import scala.concurrent.{ExecutionContext, Future}

object Sonification {
  // val emptyGraph = SynthGraph(Vector.empty, Set.empty)

  def apply(name: String): Sonification = Impl(name)

  case class MissingInput(key: String) extends RuntimeException(s"Missing sonification input for key '$key'")

  final val DefaultVariable = "<default>"

  trait Prepared {
    def play(): Synth
  }
}
trait Sonification {
  /** A user chosen name associated with the sonification. */
  var name: String

  /** The patch describing the audio signal processing of the sonification. */
  var patch: Patch

  /** OBSOLETE */
  var mapping:  Map[String, SonificationSource]

  /** Prepares the data to be sonified.
    *
    * @return a future of the prepared sonification, ready to play
    */
  def prepare()(implicit context: ExecutionContext): Future[Sonification.Prepared]

  /** Maps between logical names and variable sections used as data source. */
  var variableMap: Map[String, VariableSection]

  /** Maps between keys and user provided values. */
  var userValueMap: Map[String, Double]

  /** Debugging method that writes the sonification synth definition to a file on the user desktop. */
  def _debug_writeDef(): Unit
}
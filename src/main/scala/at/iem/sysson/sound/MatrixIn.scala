/*
 *  MatrixIn.scala
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

// OBSOLETE
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
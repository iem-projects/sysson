/*
 *  Dim.scala
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

package at.iem.sysson.graph

import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.synth.{HasSideEffect, Lazy, GE, UGenInLike, AudioRated, Optional}
import de.sciss.synth
import at.iem.sysson.sound.UGenGraphBuilder

object Dim {
  // private final val DIM_COOKIE = 0x64696D00 // "dim\0"

  //  implicit object serializer extends ImmutableSerializer[Dim] {
  //    def read(in: DataInput): Dim = {
  //      val cookie = in.readInt()
  //      require(cookie == DIM_COOKIE,
  //        s"Unexpected cookie (expected ${DIM_COOKIE.toHexString}, found ${cookie.toHexString})")
  //      val name    = in.readUTF()
  //      val minSize = ImmutableSerializer.option[Int].read(in)
  //      val maxSize = ImmutableSerializer.option[Int].read(in)
  //      Dim(name, minSize = minSize, maxSize = maxSize)
  //    }
  //
  //    def write(dim: Dim, out: DataOutput): Unit = {
  //      out.writeInt(DIM_COOKIE)
  //      out.writeUTF(dim.name)
  //      ImmutableSerializer.option[Int].write(dim.minSize, out)
  //      ImmutableSerializer.option[Int].write(dim.maxSize, out)
  //    }
  //  }

  case class Play(dim: Dim, freq: synth.GE)
    extends impl.LazyImpl with AudioRated {

    override def productPrefix = "Dim$Play"

    protected def makeUGens(b: UGenGraphBuilder): UGenInLike =
      ??? // b.addAudioSelection(dim, freq)
  }
}
/** Specification of a data source dimension
  *
  * @param variable Data source to which this dimension refers
  * @param name     Logical name by which the dimension is referred to
  */
case class Dim(variable: Var, name: String)
  extends UserInteraction {

  /** Produces a graph element which unrolls the selected range in time, using the dimension's domain value.
    *
    * @param  freq  a graph element specifying the frequency in samples per second with which to unroll.
    */
  def play(freq: GE): Dim.Play = Dim.Play(this, freq)

  def values : GE = ??? // Impl.values (this)
  def indices: GE = ??? // Impl.indices(this)

  /** Produces a graph element reflecting the low end of the range within the dimension's domain. */
  def startValue: GE = ??? // Impl.startValue(this)

  /** Produces a graph element reflecting the high end of the range within the dimension's domain. */
  def endValue: GE = ??? // Impl.endValue(this)

  /** Produces a graph element reflecting the low end of the range as index into the dimension vector. */
  def startIndex: GE = ??? // Impl.startIndex(this)

  /** Produces a graph element reflecting the high end of the range as index into the dimension vector.
    * This index is "inclusive", i.e. denotes the index corresponding to `endValue`.
    */
  def endIndex: GE = stopIndex - 1

  /** Produces a graph element reflecting the high end of the range as index into the dimension vector.
    * This index is "exclusive", i.e. denotes the index after the last included element. The index
    * corresponding to `endValue` is `endIndex` which equals `stopIndex - 1`
    */
  def stopIndex: GE = ??? // Impl.stopIndex(this)

  /** Produces a graph element reflecting the extent of this selection in the dimension's domain.
    * That is `endValue - startValue`.
    */
  def extent: GE = endValue - startValue

  /** Produces a graph element reflecting the number of samples (`stopIndex - startIndex`) covered by
    * this selection.
    */
  def size: GE = stopIndex - startIndex
}
/*
 *  UserInteraction.scala
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

package at.iem.sysson.graph

import de.sciss.synth.{HasSideEffect, UGenInLike, ScalarRated, AudioRated, GE, Lazy}
import de.sciss.synth
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import at.iem.sysson.sound.UGenGraphBuilder

trait UserInteraction extends Lazy.Expander[Unit] with HasSideEffect {
  protected final def makeUGens = ()
}

sealed trait SelectedLike extends UserInteraction {
  /** Variable proxy to select */
  def variable: VarRef
}

case class SelectedValue(variable: VarRef /*, default: */) extends SelectedLike {
  def value: GE = ???
  def index: GE = ???
}

//@deprecated("Replaced by Dim")
//object SelectedRange {
//  //  case class GE(range: SelectedRange, freq: synth.GE) extends synth.GE with AudioRated {
//  //    def expand: UGenInLike = ...
//  //  }
//
//  // XXX TODO: should _not_ be a GE, but instead provide `values` and `indices` methods.
//  case class Play(range: SelectedRange, freq: synth.GE)
//    extends impl.LazyImpl with AudioRated {
//
//    override def productPrefix = "SelectedRange$Play"
//
//    protected def makeUGens(b: UGenGraphBuilder): UGenInLike =
//      b.addAudioSelection(range, freq)
//  }
//}
//
///** An element which is a placeholder for the user selecting the range within a
//  * particular variable (dimension).
//  *
//  * @param variable the variable for which the selection is made
//  */
//@deprecated("Replaced by Dim")
//case class SelectedRange(variable: VarRef) extends SelectedLike {
//  import impl.{SelectedRangeImpl => Impl}
//
//  /** Produces a graph element which unrolls the selected range in time, using the dimension's domain value.
//    *
//    * @param  freq  a graph element specifying the frequency in samples per second with which to unroll.
//    */
//  def play(freq: GE): SelectedRange.Play = SelectedRange.Play(this, freq)
//
//  def values : GE = Impl.values (this)
//  def indices: GE = Impl.indices(this)
//
//  /** Produces a graph element reflecting the low end of the range within the dimension's domain. */
//  def startValue: GE = Impl.startValue(this)
//
//  /** Produces a graph element reflecting the high end of the range within the dimension's domain. */
//  def endValue: GE = Impl.endValue(this)
//
//  /** Produces a graph element reflecting the low end of the range as index into the dimension vector. */
//  def startIndex: GE = Impl.startIndex(this)
//
//  /** Produces a graph element reflecting the high end of the range as index into the dimension vector.
//    * This index is "inclusive", i.e. denotes the index corresponding to `endValue`.
//    */
//  def endIndex: GE = stopIndex - 1
//
//  /** Produces a graph element reflecting the high end of the range as index into the dimension vector.
//    * This index is "exclusive", i.e. denotes the index after the last included element. The index
//    * corresponding to `endValue` is `endIndex` which equals `stopIndex - 1`
//    */
//  def stopIndex: GE = Impl.stopIndex(this)
//
//  /** Produces a graph element reflecting the extent of this selection in the dimension's domain.
//    * That is `endValue - startValue`.
//    */
//  def extent: GE = endValue - startValue
//
//  /** Produces a graph element reflecting the number of samples (`stopIndex - startIndex`) covered by
//    * this selection.
//    */
//  def size: GE = stopIndex - startIndex
//}

object UserValue {
  private final val USER_COOKIE = 0x75737200  // "usr\0"

  implicit object serializer extends ImmutableSerializer[UserValue] {
    def read(in: DataInput): UserValue = {
      val cookie = in.readInt()
      require(cookie == USER_COOKIE,
        s"Unexpected cookie (expected ${USER_COOKIE.toHexString}, found ${cookie.toHexString})")
      val key     = in.readUTF()
      val default = in.readDouble()
      UserValue(key, default)
    }

    def write(v: UserValue, out: DataOutput): Unit = {
      out.writeInt(USER_COOKIE)
      out.writeUTF(v.key)
      out.writeDouble(v.default)
    }
  }

  // XXX TODO: shouldn't leak impl.LazyImpl
  case class Value(peer: UserValue) extends impl.LazyImpl with ScalarRated {
    override def productPrefix = "UserValue$Value"

    protected def makeUGens(b: UGenGraphBuilder): UGenInLike =
      b.addScalarUserValue(peer)
  }
}
case class UserValue(key: String, default: Double) extends UserInteraction {
  def value: GE = UserValue.Value(this)
}
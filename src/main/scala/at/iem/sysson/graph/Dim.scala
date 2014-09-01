/*
 *  Dim.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson.graph

import at.iem.sysson.sound.impl.MatrixPrepare
import de.sciss.synth.proc.{UGenGraphBuilder => UGB}
import de.sciss.synth.ugen.Constant
import de.sciss.synth.{ScalarRated, GE, UGenInLike, AudioRated}
import de.sciss.synth

object Dim {
  sealed trait GE extends synth.GE.Lazy /* with UGB.Input */ {
    def dim: Dim

    // type Key = Dim
    // def  key = dim
  }

  // private[sysson] def controlName(key: String, idx: Int): String = s"$$str${idx}_$key"

  // private[sysson] def key(dim: Dim): String = s"$$dim_${dim.variable.name}_${dim.name}"

  //  object Play {
  //    // private[sysson] case class Key(dim: Dim)
  //  }
  final case class Play(dim: Dim, freq: synth.GE, maxFreq: Double, interp: Int)
    extends GE with AudioRated with UGB.Input {

    override def productPrefix  = "Dim$Play"
    override def toString       = s"$dim.play($freq)"

    type Key    = Dim
    type Value  = MatrixPrepare.Value
    def  key    = dim

    protected def makeUGens: UGenInLike =
      MatrixPrepare.makeUGen(this, key = MatrixPrepare.mkKey(dim, isDim = true) /* Dim.key(dim) */,
        freq = freq, interp = interp)
  }

  //  object Values {
  //    private[sysson] def controlName(key: String): String = s"$$val_$key"
  //  }
  final case class Values(dim: Dim) extends GE with ScalarRated with UGB.Input {
    override def productPrefix  = "Dim$Values"
    override def toString       = s"$dim.values"

    type Key    = Dim
    type Value  = MatrixPrepare.Value
    def  key    = dim

    protected def makeUGens: UGenInLike =
      MatrixPrepare.makeUGen(this, key = MatrixPrepare.mkKey(dim, isDim = true) /* Dim.key(dim) */,
        freq = 0f, interp = 0)
  }

  //  final case class IndexRange(dim: Dim) extends GE with ScalarRated {
  //    override def productPrefix  = "Dim$IndexRange"
  //    override def toString       = s"Dim.IndexRange($dim)"
  //
  //    protected def makeUGens: UGenInLike = {
  //      val b     = UGB.get
  //      // val aural = AuralSonificationOLD.current()
  //      val key: String = ... //   = aural.attributeKey(this)
  //      proc.graph.Attribute.kr(key)
  //    }
  //  }
}
/** Specification of a data source dimension
  *
  * @param variable Data source to which this dimension refers
  * @param name     Logical name by which the dimension is referred to
  */
final case class Dim(variable: Var, name: String)
  extends UserInteraction with UGB.Key {

  /** Produces a graph element which unrolls the selected range in time, using the dimension's domain value.
    *
    * @param  freq    a graph element specifying the frequency in samples per second with which to unroll.
    * @param  maxFreq estimated maximum value that `freq` may have at any point. `0.0` if unknown
    * @param  interp  value interpolation: 1 none, 2 linear, 4 cubic
    */
  def play(freq: GE, maxFreq: Double = 0.0, interp: Int = 1): Dim.Play = {
    val maxFreq1 = freq match {
      case Constant(c)  => c
      case _            => maxFreq
    }
    Dim.Play(this, freq = freq, maxFreq = maxFreq1, interp = interp)
  }

  def values : Dim.Values = Dim.Values(this)

  // def indices: GE = ...

  //  /** Produces a graph element reflecting the low end of the range within the dimension's domain. */
  //  def startValue: GE = ...
  //
  //  /** Produces a graph element reflecting the high end of the range within the dimension's domain. */
  //  def endValue: GE = ...

  //  /** Produces a graph element reflecting the low end of the range as index into the dimension vector. */
  //  def startIndex: GE = Dim.IndexRange(this) \ 0
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
  //  def stopIndex: GE = Dim.IndexRange(this) \ 1

  //  /** Produces a graph element reflecting the extent of this selection in the dimension's domain.
  //    * That is `endValue - startValue`.
  //    */
  //  def extent: GE = endValue - startValue

  /** Produces a graph element reflecting the number of samples (`stopIndex - startIndex`) covered by
    * this selection.
    */
  def size: GE = ??? // stopIndex - startIndex
}
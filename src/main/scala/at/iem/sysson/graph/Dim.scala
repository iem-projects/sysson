/*
 *  Dim.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package graph

import at.iem.sysson.sound.impl.MatrixPrepare
import de.sciss.synth
import de.sciss.synth.proc.{UGenGraphBuilder => UGB}
import de.sciss.synth.ugen.Constant
import de.sciss.synth.{GE, ScalarRated, UGenInLike}

object Dim {
  final case class Play(dim: Dim, freq: synth.GE, maxFreq: Double, interp: Int)
    extends MatrixPrepare.DimGE with MatrixPrepare.PlayGE {

    override def productPrefix  = "Dim$Play"
    override def toString       = s"$dim.play($freq)"
  }

  final case class Values(dim: Dim)
    extends MatrixPrepare.DimGE with MatrixPrepare.ValuesGE {

    override def productPrefix  = "Dim$Values"
    override def toString       = s"$dim.values"
  }

  final case class Size(dim: Dim)
    extends synth.GE.Lazy with UGB.Input with ScalarRated with UGB.Key {

    override def productPrefix  = "Dim$Size"
    override def toString       = s"Dim.Size($dim)"

    type Key      = Size
    type Value    = UGB.Unit
    def key: Key  = this

    private[sysson] def ctlName: String = s"$$sz_dim_${dim.variable.name}_${dim.name}"

    protected def makeUGens: UGenInLike = {
      import synth._
      val b = UGB.get
      b.requestInput(this)
      ctlName.ir(0f)
    }
  }
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
  //  def startIndex: GE = Dim.Size(this) \ 0
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
  //  def stopIndex: GE = Dim.Size(this) \ 1

  //  /** Produces a graph element reflecting the extent of this selection in the dimension's domain.
  //    * That is `endValue - startValue`.
  //    */
  //  def extent: GE = endValue - startValue

  /** Produces a graph element reflecting the number of samples (`stopIndex - startIndex`) covered by
    * this selection.
    */
  def size: Dim.Size = Dim.Size(this) // stopIndex - startIndex
}
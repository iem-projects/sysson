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

import de.sciss.synth.proc.impl.StreamBuffer
import de.sciss.synth.proc.{UGenGraphBuilder => UGB}
import de.sciss.synth.ugen.Constant
import de.sciss.synth.{ScalarRated, proc, GE, UGenInLike, AudioRated}
import de.sciss.synth

object Dim {
  sealed trait GE extends synth.GE.Lazy /* with UGB.Input */ {
    def dim: Dim

    // type Key = Dim
    // def  key = dim
  }

  private[sysson] def controlName(key: String, idx: Int): String = s"$$str${idx}_$key"

  object Play {
    // private[sysson] case class Key(dim: Dim)
    private[sysson] def key(dim: Dim): String = s"$$dim_${dim.variable.name}_${dim.name}"
  }
  final case class Play(dim: Dim, freq: synth.GE, maxFreq: Double, interp: Int)
    extends GE with AudioRated with UGB.Input {

    private val maxFreq1 = freq match {
      case Constant(c)  => c
      case _            => maxFreq
    }

    override def productPrefix  = "Dim$Play"
    override def toString       = s"$dim.play($freq)"

    type Key    = Dim // UGB.AttributeKey
    type Value  = UGB.Input.Stream.Value
    def  key    = dim

    protected def makeUGens: UGenInLike = {
      val b     = UGB.get
      // val spec  = UGB.Input.Stream.Spec(maxSpeed = maxSpeed1, interp = interp)
      val info  = b.requestInput(this)

      val key   = Play.key(dim)

      import synth._
      import ugen._
      val bufSr     = SampleRate.ir  // note: VDiskIn uses server sample rate as scale base
      val speed     = freq / bufSr

      // proc.graph.VDiskIn.ar(key, speed = speed, interp = 1 /*, maxSpeed = maxSpeed */)  // need server.sampleRate for maxSpeed

      val idx           = /* if (spec.isEmpty) 0 else */ info.specs.size - 1
      // val (numCh, idx)  = b.addStreamIn(key, info)
      val ctlName       = controlName(key, idx)
      //      val ctl           = ctlName.ir(Seq(0, 0))
      //      val buf           = ctl \ 0
      val buf           = ctlName.ir(0)
      val numCh         = info.numChannels
      assert(numCh == 1)
      // println(s"Dim.Play - numChannels = $numCh")
      StreamBuffer.makeUGen(key = key, idx = idx, buf = buf, numChannels = numCh, speed = speed, interp = interp)
    }
  }

  final case class Values(dim: Dim) extends GE with ScalarRated {
    override def productPrefix  = "Dim$Values"
    override def toString       = s"$dim.values"

    protected def makeUGens: UGenInLike = {
      val b     = UGB.get
      // val aural = AuralSonificationOLD.current()
      val key: String = ??? //  = aural.attributeKey(this)
      proc.graph.attribute(key).ir
    }
  }

  final case class IndexRange(dim: Dim) extends GE with ScalarRated {
    override def productPrefix  = "Dim$IndexRange"
    override def toString       = s"Dim.IndexRange($dim)"

    protected def makeUGens: UGenInLike = {
      val b     = UGB.get
      // val aural = AuralSonificationOLD.current()
      val key: String = ??? //   = aural.attributeKey(this)
      proc.graph.attribute(key).ir
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
    * @param  freq  a graph element specifying the frequency in samples per second with which to unroll.
    */
  def play(freq: GE, maxFreq: Double = 0.0, interp: Int = 1): Dim.Play =
    Dim.Play(this, freq = freq, maxFreq = maxFreq, interp = interp)

  def values : Dim.Values = Dim.Values(this)

  // def indices: GE = ...

  //  /** Produces a graph element reflecting the low end of the range within the dimension's domain. */
  //  def startValue: GE = ...
  //
  //  /** Produces a graph element reflecting the high end of the range within the dimension's domain. */
  //  def endValue: GE = ...

  /** Produces a graph element reflecting the low end of the range as index into the dimension vector. */
  def startIndex: GE = Dim.IndexRange(this) \ 0

  /** Produces a graph element reflecting the high end of the range as index into the dimension vector.
    * This index is "inclusive", i.e. denotes the index corresponding to `endValue`.
    */
  def endIndex: GE = stopIndex - 1

  /** Produces a graph element reflecting the high end of the range as index into the dimension vector.
    * This index is "exclusive", i.e. denotes the index after the last included element. The index
    * corresponding to `endValue` is `endIndex` which equals `stopIndex - 1`
    */
  def stopIndex: GE = Dim.IndexRange(this) \ 1

  //  /** Produces a graph element reflecting the extent of this selection in the dimension's domain.
  //    * That is `endValue - startValue`.
  //    */
  //  def extent: GE = endValue - startValue

  /** Produces a graph element reflecting the number of samples (`stopIndex - startIndex`) covered by
    * this selection.
    */
  def size: GE = stopIndex - startIndex
}
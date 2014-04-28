/*
 *  Var.scala
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

import de.sciss.synth
import de.sciss.synth.{proc, ScalarRated, UGenInLike, AudioRated}
import de.sciss.serial.ImmutableSerializer
import at.iem.sysson.sound.AuralSonification

object Var {
  sealed trait GE extends synth.GE.Lazy with SonificationElement {
    def variable: Var

    // def axis(dim: Dim): Axis
  }

  case class Play(variable: Var, time: Dim.Play)
    extends /* impl.LazyImpl with */ GE with AudioRated {

    override def productPrefix  = "Var$Play"
    override def toString       = s"$variable.play($time)"

    protected def makeUGens: UGenInLike = {
      val aural = AuralSonification.current()
      val key   = aural.attributeKey(this)
      import synth.ugen._
      val bufSr     = SampleRate.ir  // note: VDiskIn uses server sample rate as scale base
      val speed     = time.freq / bufSr
      // val maxSpeed
      proc.graph.VDiskIn.ar(key, speed = speed, interp = 1 /*, maxSpeed = maxSpeed */)  // XXX TODO: need server.sampleRate for maxSpeed
    }

    def axis(dim: Dim): Var.Axis = {
      require (dim.variable == variable, s"Dimension ${dim.name} does not belong to variable $variable")
      Var.Axis(this, dim.name)
    }
  }

  case class Values(variable: Var) extends GE with ScalarRated {

    override def productPrefix  = "Var$Values"
    override def toString       = s"$variable.values"

    protected def makeUGens: UGenInLike = {
      val aural = AuralSonification.current()
      val key   = aural.attributeKey(this)
      proc.graph.attribute(key).ir
    }
  }

  /** Declares a new sonification variable (data source).
    *
    * @param name         Logical name by which the source is referred to
    * @param higherRank   Whether a matrix rank higher than `dimensions.size` is permitted
    */
  def apply(name: String, /* dims: Vec[Dim], */ higherRank: Boolean = false): Var =
    Impl(name, higherRank)

  def unapply(vr: Var): Option[(String, /* Vec[Dim], */ Boolean /*, Vec[Var.Op] */)] = Some(
    vr.name, vr.higherRank
  )

  // implicit def serializer: ImmutableSerializer[Var] = impl.VarImpl.serializer

  // ---- axis -----

  // XXX TODO: should be common trait with SelectedRange (values, indices, extent, size, startValue, ...)

  object Axis {
    case class Values(axis: Var.Axis)
      extends synth.GE.Lazy /* impl.LazyImpl */ with SonificationElement with ScalarRated {

      override def productPrefix = "Var$Axis$Values"

      override def toString = s"$axis.values"

      // protected def makeUGens(b: UGenGraphBuilderOLD): UGenInLike = b.addScalarAxis(axis.playing, axis.ref)

      protected def makeUGens: UGenInLike = {
        val key = AuralSonification.current().attributeKey(this)
        proc.graph.attribute(key).ir
      }
    }
  }

  /** A reference to a dimensional axis with respect to a variable section.
    * The difference between this and for instance `SelectDim` is that the
    * graph element producing methods such as `values` and `indices` produce
    * multi-channel signals which correctly align with the underlying variable section.
    * This allows signal processing which combines each sample value from a
    * variable with the corresponding axes elements.
    */
  case class Axis(variable: Var.Play, dim: String) {
    override def productPrefix = "Var$Axis"

    override def toString = s"$variable.axis($dim)"

    def values    : synth.GE = Axis.Values(this)

    def indices   : synth.GE = ???

    def startValue: synth.GE = ???

    def endValue  : synth.GE = ???
  }

  // -------- VarImpl --------

  private final case class Impl(name: String, higherRank: Boolean)
    extends Var {

    override def productPrefix = "Var"

    def dim(name: String): Dim = Dim(this, name)

    // def ir: Var.GE = ...
    // def kr: Var.GE = ...

    def values: Var.Values = Var.Values(this)

    def play(time: Dim.Play): Var.Play = Var.Play(this, time)
  }
}
trait Var extends UserInteraction {
  /** Logical name by which the source is referred to */
  def name: String

  def dim(name: String): Dim

  /** Whether a matrix rank higher than `dimensions.size` is permitted */
  def higherRank: Boolean

  // def ir: Var.GE
  // def kr: Var.GE

  def values: Var.Values

  /** A special sectioning which unrolls one of the variable dimensions in time. */
  def play(time: Dim.Play): Var.Play
}

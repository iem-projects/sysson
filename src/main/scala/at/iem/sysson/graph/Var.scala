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

import at.iem.sysson.sound.impl.MatrixPrepare
import de.sciss.synth
import de.sciss.synth.proc.{UGenGraphBuilder => UGB}
import de.sciss.synth.{proc, ScalarRated, UGenInLike}

object Var {
  final case class Play(variable: Var, time: Dim.Play, interp: Int)
    extends MatrixPrepare.VarGE with MatrixPrepare.PlayGE with UGB.Input {

    override def productPrefix  = "Var$Play"
    override def toString       = s"$variable.play($time)"

    type Key    = Dim
    def  key    = time.dim

    protected def freq = time.freq
    def maxFreq = time.maxFreq

    private[sysson] def dimOption: Option[Dim] = Some(time.dim)

    //    protected def makeUGens: UGenInLike =
    //      MatrixPrepare.makeUGenOLD(this, key = MatrixPrepare.mkKeyOLD(time.dim, isDim = false),
    //        freq = time.freq, interp = interp)

    def axis(dim: Dim): Var.Axis = {
      require (dim.variable == variable, s"Dimension ${dim.name} does not belong to variable $variable")
      Var.Axis(this, dim.name)
    }
  }

  final case class Values(variable: Var)
    extends MatrixPrepare.VarGE with MatrixPrepare.ValuesGE with UGB.Input {

    override def productPrefix  = "Var$Values"
    override def toString       = s"$variable.values"

    type Key    = Var
    def  key    = variable

    private[sysson] def dimOption: Option[Dim] = None
  }

  //  /** Declares a new sonification variable (data source).
  //    *
  //    * @param name         Logical name by which the source is referred to
  //    * @param higherRank   Whether a matrix rank higher than `dimensions.size` is permitted
  //    */
  //  def apply(name: String, /* dims: Vec[Dim], */ higherRank: Boolean = false): Var =
  //    Impl(name, higherRank)
  //
  //  def unapply(vr: Var): Option[(String, /* Vec[Dim], */ Boolean /*, Vec[Var.Op] */)] = Some(
  //    vr.name, vr.higherRank
  //  )

  // implicit def serializer: ImmutableSerializer[Var] = impl.VarImpl.serializer

  // ---- axis -----

  // XXX TODO: should be common trait with SelectedRange (values, indices, extent, size, startValue, ...)

  object Axis {
    final case class Values(axis: Var.Axis)
      extends synth.GE.Lazy /* impl.LazyImpl */ /* with SonificationElement */ with ScalarRated {

      override def productPrefix = "Var$Axis$Values"

      override def toString = s"$axis.values"

      // protected def makeUGens(b: UGenGraphBuilderOLD): UGenInLike = b.addScalarAxis(axis.playing, axis.ref)

      protected def makeUGens: UGenInLike = {
        val b         = UGB.get
        val keyComp: String = ??? //   = AuralSonificationOLD.current().attributeKey(this)
        val keySp     = keyComp.split(";")
        val key       = keySp(0)
        val axisSize  = keySp(1).toInt
        val div       = keySp(2).toInt
        val axisSignal= proc.graph.Attribute.ir(key)
        println(s"axisSize = $axisSize, div = $div")
        Vector.tabulate(axisSize * div)(i => axisSignal \ (i/div)): synth.GE
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
  final case class Axis(variable: Var.Play, dim: String) {
    override def productPrefix = "Var$Axis"

    override def toString = s"$variable.axis($dim)"

    def values    : synth.GE = Axis.Values(this)

    // def indices   : synth.GE = ...
    // def startValue: synth.GE = ...
    // def endValue  : synth.GE = ...

    def asDim: Dim = Dim(variable.variable, dim)
  }

  //  // -------- VarImpl --------
  //
  //  private final case class Impl(name: String, higherRank: Boolean)
  //    extends Var {
  //
  //    override def productPrefix = "Var"
  //
  //    def dim(name: String): Dim = Dim(this, name)
  //
  //    // def ir: Var.GE = ...
  //    // def kr: Var.GE = ...
  //
  //    def values: Var.Values = Var.Values(this)
  //
  //    def play(time: Dim.Play, interp: Int): Var.Play = Var.Play(this, time, interp)
  //  }
}
// XXX TODO - remove obsolete `higherRank` argument, once we don't need session compatibility
final case class Var(name: String, higherRank: Boolean = true) extends UserInteraction with UGB.Key {
  //  /** Logical name by which the source is referred to */
  //  def name: String

  def dim(name: String): Dim = Dim(this, name)

  //  /** Whether a matrix rank higher than `dimensions.size` is permitted */
  //  def higherRank: Boolean

  // def ir: Var.GE
  // def kr: Var.GE

  def values: Var.Values = Var.Values(this)

  /** A special sectioning which unrolls one of the variable dimensions in time. */
  def play(time: Dim.Play, interp: Int = 1): Var.Play = Var.Play(this, time, interp)
}

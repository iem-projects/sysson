/*
 *  Var.scala
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
import at.iem.sysson.sound.impl.MatrixPrepare.ShapeAndIndex
import de.sciss.synth
import de.sciss.synth.proc.{UGenGraphBuilder => UGB}
import de.sciss.synth.{ScalarRated, UGenInLike}
import org.scalautils.TypeCheckedTripleEquals

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
      import TypeCheckedTripleEquals._
      require (dim.variable === variable, s"Dimension ${dim.name} does not belong to variable $variable")
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

  final case class Size(variable: Var)
    extends synth.GE.Lazy with UGB.Input with ScalarRated with UGB.Key {

    override def productPrefix  = "Var$Size"
    override def toString       = s"Var.Size($variable)"

    type Key      = Size
    type Value    = UGB.Unit
    def key: Key  = this

    private[sysson] def ctlName: String = s"$$sz_var_$variable.name}"

    protected def makeUGens: UGenInLike = {
      import synth._
      val b = UGB.get
      b.requestInput(this)
      ctlName.ir(0f)
    }
  }

  // ---- axis -----

  // XXX TODO: should be common trait with SelectedRange (values, indices, extent, size, startValue, ...)

  object Axis {
    final case class Key(stream: Dim, axis: String) extends UGB.Key {
      override def productPrefix = "Var.Axis.Key"
      override def toString = s"$productPrefix(stream = $stream, axis = $axis)"
    }
    final case class Values(axis: Var.Axis)
      extends MatrixPrepare.GE with ScalarRated {

      override def productPrefix = "Var$Axis$Values"

      override def toString = s"$axis.values"

      type Key = Axis.Key
      def key: Key = Axis.Key(stream = axis.variable.time.dim, axis = axis.dim)

      type Value = ShapeAndIndex

      def variable: Var = axis.variable.variable

      protected def makeUGens: UGenInLike = {
        val b         = UGB.get
        // cf. VariableAxesAssociations.txt
        val shapeAndIndex @ ShapeAndIndex(shape, streamIdx, axisIdx) = b.requestInput(this)
        val shapeRed  = shape.updated(streamIdx, 1)               // same as removal when applying `product`!
        val divL      = (1L /: shapeRed.drop(axisIdx + 1))(_ * _) // note: empty product == 1
        val axisSize  = shapeRed(axisIdx)
        // note: matSize is generally smaller than shapeRed.product, because
        // we can exploit signal repetition in multi-channel matching in SuperCollider
        val matSizeL  = axisSize * divL
        if (matSizeL > 0xFFFFFF) sys.error(s"Matrix too large for axis value generation ($matSizeL)")
        val div       = divL.toInt
        val matSize   = matSizeL.toInt
        val dimVals   = axis.asDim.values
        logDebug(s"$this: $shapeAndIndex; axisSize = $axisSize, div = $div, matSize = $matSize")
        Vector.tabulate(matSize)(i => dimVals \ (i/div)): synth.GE
      }
    }
  }

  /** A reference to a dimensional axis with respect to a variable section.
    * The difference between this and for instance `Dim` is that the
    * graph element producing methods such as `values` and `indices` produce
    * multi-channel signals which correctly align with the underlying variable section.
    * This allows signal processing which combines each sample value from a
    * variable with the corresponding axes elements.
    */
  final case class Axis(variable: Var.Play, dim: String) {
    override def productPrefix = "Var$Axis"

    override def toString = s"""$variable.axis($dim)"""

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

  override def toString = s"""$productPrefix("$name")"""

  def dim(name: String): Dim = Dim(this, name)

  //  /** Whether a matrix rank higher than `dimensions.size` is permitted */
  //  def higherRank: Boolean

  // def ir: Var.GE
  // def kr: Var.GE

  def values: Var.Values = Var.Values(this)

  /** A special sectioning which unrolls one of the variable dimensions in time. */
  def play(time: Dim.Play, interp: Int = 1): Var.Play = Var.Play(this, time, interp)

  def size: Var.Size = Var.Size(this)
}

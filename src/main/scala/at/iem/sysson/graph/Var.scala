package at.iem.sysson.graph

import de.sciss.synth
import de.sciss.synth.{ScalarRated, UGenInLike, AudioRated}
import at.iem.sysson._
import de.sciss.serial.ImmutableSerializer
import at.iem.sysson.sound.UGenGraphBuilder

object Var {
  trait GE extends synth.GE {
    def variable: Var

    // XXX TODO should be axis().values and axis().indices
    // def axisValues(ref: VarRef): GE

    def axis(ref: VarRef): Axis
  }

  case class Play(variable: Var, time: SelectedRange.Play)
    extends impl.LazyImpl with AudioRated {

    override def productPrefix = "Var$Play"

    def axis(ref: VarRef): Var.Axis = Var.Axis(this, ref)

    protected def makeUGens(b: UGenGraphBuilder): UGenInLike = b.addAudioVariable(this)
  }

  // def apply(): Var = impl.VarImpl.Default

  /** Declares a new sonification variable (data source).
    *
    * @param name         Logical name by which the source is referred to
    * @param dims         List of specifications of required dimensions
    * @param higherRank   Whether a matrix rank higher than `dimensions.size` is permitted
    */
  def apply(name: String, dims: Vec[Dim], higherRank: Boolean = false, operations: Vec[Var.Op] = Vec.empty): Var =
    Impl(name, dims, higherRank, operations)

  implicit def serializer: ImmutableSerializer[Var] = impl.VarImpl.serializer

  // ---- axis -----

  // XXX TODO: should be common trait with SelectedRange (values, indices, extent, size, startValue, ...)

  object Axis {
    case class Values(axis: Var.Axis) extends impl.LazyImpl with ScalarRated {
      override def productPrefix = "Var$Axis$Values"

      protected def makeUGens(b: UGenGraphBuilder): UGenInLike =
        b.addScalarAxis(axis.playing, axis.ref)
    }
  }

  /** A reference to a dimensional axis with respect to a variable section.
    * The difference between this and for instance SelectRange is that the
    * graph element producing methods such as `values` and `indices` produce
    * multi-channel signals which correctly align with the underlying variable section.
    * This allows signal processing which combines each sample value from a
    * variable with the corresponding axes elements.
    */
  case class Axis(playing: Var.Play, ref: VarRef) {
    override def productPrefix = "Var$Axis"

    def values    : synth.GE = Axis.Values(this)

    def indices   : synth.GE = ???

    def startValue: synth.GE = ???

    def endValue  : synth.GE = ???
  }



  // ---- operations ----

  sealed trait Op

  sealed trait Reduction extends Op {
    def variable: VarRef
  }

  final case class Select (selection: SelectedLike) extends Reduction {
    override def productPrefix = "Var$Select"

    def variable: VarRef = selection.variable
  }
  final case class Average(variable: VarRef) extends Reduction

  // -------- VarImpl --------

  private final case class Impl(name: String, dims: Vec[Dim], higherRank: Boolean,
                                              operations: Vec[Var.Op]) extends Var {
    override def productPrefix = "Var"

    private def select1(selection: SelectedLike): Impl = {
      requireUnusedReduction(selection.variable)
      copy(operations = operations :+ Var.Select(selection))
    }

    private def requireUnusedReduction(v: VarRef): Unit =
      require(!operations.exists {
        case r: Var.Reduction if r.variable == v => true
        case _ => false
      }, s"Dimension $v has already been selected or reduced")

    def select(selections: SelectedLike*): Var = (this /: selections)(_ select1 _)

    def average(dim: VarRef): Var = {
      requireUnusedReduction(dim)
      copy(operations = operations :+ Var.Average(dim))
    }

    def ir: Var.GE = ???
    def kr: Var.GE = ???

    def play(time: SelectedRange.Play): Var.Play = Var.Play(this, time)
  }
}
trait Var extends UserInteraction {
  /** Logical name by which the source is referred to */
  def name: String
  /** List of specifications of required dimensions */
  def dims: Vec[Dim]
  /** Whether a matrix rank higher than `dimensions.size` is permitted */
  def higherRank: Boolean

  def select(selections: SelectedLike*): Var
  def average(dim: VarRef): Var

  def ir: Var.GE
  def kr: Var.GE

  /** A special sectioning which unrolls one of the variable dimensions in time. */
  def play(time: SelectedRange.Play): Var.Play

  /** The operations performed on the original input variable, such as taking slices,
    * averaging over a dimension, etc.
    */
  def operations: Vec[Var.Op]
}

package at.iem.sysson
package graph
package impl

import de.sciss.synth.{GE, ScalarRated, UGenInLike}
import at.iem.sysson.graph.Var
import at.iem.sysson.sound.UGenGraphBuilder

object VarImpl {
  def Default: Var = Impl(Vec.empty)

  private final case class Impl(operations: Vec[Var.Op]) extends Var {
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

    def play(time: SelectedRange.Playing): Var.Playing = new PlayingImpl(this, time)
  }

  private final case class AxisImpl(playing: Var.Playing, ref: VarRef) extends Var.Axis {
    def values    : GE = new AxisValuesImpl(this)

    def indices   : GE = ???

    def startValue: GE = ???

    def endValue  : GE = ???
  }

  private final case class AxisValuesImpl(axis: AxisImpl) extends LazyImpl with ScalarRated {
    protected def makeUGens(b: UGenGraphBuilder): UGenInLike =
      b.addScalarAxis(axis.playing, axis.ref)
  }

  private final case class PlayingImpl(variable: Impl, time: SelectedRange.Playing)
    extends LazyImpl with Var.Playing {

    def axis(ref: VarRef): Var.Axis = new AxisImpl(this, ref)

    protected def makeUGens(b: UGenGraphBuilder): UGenInLike =
      b.addAudioVariable(this)
  }
}

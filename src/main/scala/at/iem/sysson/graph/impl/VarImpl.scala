package at.iem.sysson
package graph
package impl

import de.sciss.synth.GE

object VarImpl {
  def Default: Var = Impl(Vec.empty)

  private sealed trait Op {

  }

  private sealed trait Reduction extends Op {
    def variable: VarRef
  }

  private case class Select (selection: SelectedLike) extends Reduction {
    def variable: VarRef = selection.variable
  }
  private case class Average(variable: VarRef) extends Reduction

  private final case class Impl(ops: Vec[Op]) extends Var {
    private def select1(selection: SelectedLike): Impl = {
      requireUnusedReduction(selection.variable)
      copy(ops = ops :+ Select(selection))
    }

    private def requireUnusedReduction(v: VarRef): Unit =
      require(!ops.exists {
        case r: Reduction if r.variable == v => true
        case _ => false
      }, s"Dimension $v has already been selected or reduced")

    def select(selections: SelectedLike*): Var = (this /: selections)(_ select1 _)

    def average(dim: VarRef): Var = {
      requireUnusedReduction(dim)
      copy(ops = ops :+ Average(dim))
    }

    def ir: Var.GE = ???

    def ar(time: SelectedRange.GE): Var.AudioGE = ???
  }

  //private final class GEImpl
}

package at.iem.sysson.graph

import de.sciss.synth.{UGenInLike, AudioRated, GE, Lazy}
import de.sciss.synth

trait UserInteraction extends Lazy.Expander[Unit] {
  protected final def makeUGens = ()
}

sealed trait SelectedLike extends UserInteraction {
  /** Variable proxy to select */
  def variable: VarRef
}

case class SelectedValue(variable: VarRef /*, default: */) extends SelectedLike {
  def ir: GE = ???
  def kr: GE = ???
}

object SelectedRange {
  case class GE(range: SelectedRange, freq: synth.GE) extends synth.GE with AudioRated {
    def expand: UGenInLike = ???
  }
}
case class SelectedRange(variable: VarRef) extends SelectedLike {
  def ar(freq: GE): SelectedRange.GE = SelectedRange.GE(this, freq)
  def min: GE = ???
  def max: GE = ???
}
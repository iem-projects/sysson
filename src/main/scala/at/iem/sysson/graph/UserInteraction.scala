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
  def value: GE = ???
  def index: GE = ???
}

object SelectedRange {
  //  case class GE(range: SelectedRange, freq: synth.GE) extends synth.GE with AudioRated {
  //    def expand: UGenInLike = ...
  //  }

  trait Playing extends synth.GE with AudioRated {
    def range: SelectedRange
    def freq: synth.GE
  }
}

/** An element which is a placeholder for the user selecting the range within a
  * particular variable (dimension).
  *
  * @param variable the variable for which the selection is made
  */
case class SelectedRange(variable: VarRef) extends SelectedLike {
  import impl.{SelectedRangeImpl => Impl}

  /** Produces a graph element which unrolls the selected range in time, using the dimension's domain value.
    *
    * @param  freq  a graph element specifying the frequency in samples per second with which to unroll.
    */
  def play(freq: GE): SelectedRange.Playing = Impl.play(this, freq)

  def values : GE = Impl.values (this)
  def indices: GE = Impl.indices(this)

  /** Produces a graph element reflecting the low end of the range within the dimension's domain. */
  def startValue: GE = Impl.startValue(this)

  /** Produces a graph element reflecting the high end of the range within the dimension's domain. */
  def endValue: GE = Impl.endValue(this)

  /** Produces a graph element reflecting the low end of the range as index into the dimension vector. */
  def startIndex: GE = Impl.startIndex(this)

  /** Produces a graph element reflecting the high end of the range as index into the dimension vector.
    * This index is "inclusive", i.e. denotes the index corresponding to `endValue`.
    */
  def endIndex: GE = stopIndex - 1

  /** Produces a graph element reflecting the high end of the range as index into the dimension vector.
    * This index is "exclusive", i.e. denotes the index after the last included element. The index
    * corresponding to `endValue` is `endIndex` which equals `stopIndex - 1`
    */
  def stopIndex: GE = Impl.stopIndex(this)

  /** Produces a graph element reflecting the extent of this selection in the dimension's domain.
    * That is `endValue - startValue`.
    */
  def extent: GE = endValue - startValue

  /** Produces a graph element reflecting the number of samples (`stopIndex - startIndex`) covered by
    * this selection.
    */
  def size: GE = stopIndex - startIndex
}
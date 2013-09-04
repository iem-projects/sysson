package at.iem.sysson
package graph
package impl

import de.sciss.synth
import de.sciss.synth.{ScalarRated, ControlRated, GE, UGenInLike}
import at.iem.sysson.sound.UGenGraphBuilder

object SelectedRangeImpl {
  def play(range: SelectedRange, freq: synth.GE): SelectedRange.Playing = new PlayingImpl(range, freq)

  def startValue(range: SelectedRange): GE = ???
  def endValue  (range: SelectedRange): GE = ???

  def startIndex(range: SelectedRange): GE = ???
  def stopIndex (range: SelectedRange): GE = ???

  def values    (range: SelectedRange): GE = new ValuesImpl(range)
  def indices   (range: SelectedRange): GE = ???

  private final case class PlayingImpl(range: SelectedRange, freq: synth.GE)
    extends SelectedRange.Playing with GE.Lazy {

    protected def makeUGens: UGenInLike = ???
  }

  private final case class ValuesImpl(range: SelectedRange) extends LazyImpl with ScalarRated {

    protected def makeUGens(b: UGenGraphBuilder): UGenInLike =
      b.addScalarSelection(range)
  }
}

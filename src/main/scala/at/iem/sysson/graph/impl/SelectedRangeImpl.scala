package at.iem.sysson
package graph
package impl

import de.sciss.synth
import de.sciss.synth.{GE, UGenInLike}

object SelectedRangeImpl {
  def play(range: SelectedRange, freq: synth.GE): SelectedRange.Playing = new PlayingImpl(range, freq)

  def startValue(range: SelectedRange): GE = ???
  def endValue  (range: SelectedRange): GE = ???

  def startIndex(range: SelectedRange): GE = ???
  def stopIndex (range: SelectedRange): GE = ???

  def ir        (range: SelectedRange): GE = ???
  def kr        (range: SelectedRange): GE = ???

  private final case class PlayingImpl(range: SelectedRange, freq: synth.GE) extends SelectedRange.Playing {
    def expand: UGenInLike = {
      ???
    }
  }


}

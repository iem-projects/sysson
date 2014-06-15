package at.iem.sysson.graph

import de.sciss.synth
import de.sciss.synth.{HasSideEffect, UGenInLike, GE, Rate, control, audio}

object Elapsed {
  def :=(in: Dim.Play): Elapsed = ar(in)
  def ar(in: Dim.Play, freeSelf: Boolean = true): Elapsed = apply(audio  , in, freeSelf = freeSelf)
  def kr(in: Dim.Play, freeSelf: Boolean = true): Elapsed = apply(control, in, freeSelf = freeSelf)
}
final case class Elapsed(rate: Rate, in: Dim.Play, freeSelf: Boolean)
  extends GE.Lazy with /* SonificationElement with */ HasSideEffect {

  /* Important: in order for `Dim.IndexRange` to be found in the aural
   * sonification, the "expansion" should happen immediately, not late.
   * Thus it must be created now.
   */
  private val numFrames = in.dim.size

  protected def makeUGens: UGenInLike = {
    import synth.{freeSelf => _, _}
    import synth.ugen._
    // val bufSr     = SampleRate.ir  // note: VDiskIn uses server sample rate as scale base
    val frame     = Sweep(rate, 0, in.freq) // speed = increment _per second_
    val _ratio    = frame / numFrames
    val ratioK    = if (rate == audio) A2K.kr(_ratio) else _ratio
        in.freq      .poll(0, "freq")
        frame        .poll(2, "frame")
        numFrames    .poll(0, "numFrames")
        // ratioK       .poll(2, "ratioK")
    if (freeSelf) FreeSelf.kr(ratioK >= 1)
    // XXX TODO: SendTrig with values from `in`
    _ratio
  }
}

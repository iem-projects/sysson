package at.iem.sysson.graph

import at.iem.sysson.sound.AuralSonification
import de.sciss.synth
import de.sciss.synth.{HasSideEffect, UGenInLike, GE, Rate, control, audio}

object Elapsed {
  def :=(in: Dim.Play): Elapsed = ar(in)
  def ar(in: Dim.Play, freeSelf: Boolean = true): Elapsed = apply(audio  , in, freeSelf = freeSelf)
  def kr(in: Dim.Play, freeSelf: Boolean = true): Elapsed = apply(control, in, freeSelf = freeSelf)
}
final case class Elapsed(rate: Rate, in: Dim.Play, freeSelf: Boolean)
  extends GE.Lazy with SonificationElement with HasSideEffect {

  protected def makeUGens: UGenInLike = {
    val aural     = AuralSonification.current()
    val key       = aural.attributeKey(this)
    import synth.{freeSelf => _, _}
    import synth.ugen._
    val bufSr     = SampleRate.ir  // note: VDiskIn uses server sample rate as scale base
    val speed     = in.freq / bufSr
    val frame     = Sweep(rate, 0, speed)
    val numFrames = key.ir(1)
    val ratio     = frame / numFrames
    val ratioK    = if (rate == audio) A2K.kr(ratio) else ratio
    if (freeSelf) FreeSelf.kr(ratioK >= 1)
    // XXX TODO: SendTrig with values from `in`
    ratio
  }
}

package at.iem.sysson.graph

import de.sciss.synth
import de.sciss.synth.proc.UGenGraphBuilder
import de.sciss.synth.{proc, HasSideEffect, UGenInLike, GE, Rate, control, audio}

/** A special graph element that measures the progress of a playing dimension.
  * Additionally it can stop the playing synth when that progress has reached 100%.
  */
object Elapsed {
  /** Convenient syntax if the output of the element is not used.
    * E.g. `Elapsed := aPlayingDim` will monitor the dimension and
    * stop the synth when its time has elapsed.
    */
  def :=(in: Dim.Play): Elapsed = ar(in)

  /** @param in         the playing dimension
    * @param terminate   if `true`, stops the synth when 100% is reached
    */
  def ar(in: Dim.Play, terminate: Boolean = true): Elapsed = apply(audio  , in, terminate = terminate)

  /** @param in         the playing dimension
    * @param terminate   if `true`, stops the synth when 100% is reached
    */
  def kr(in: Dim.Play, terminate: Boolean = true): Elapsed = apply(control, in, terminate = terminate)
}

/** A special graph element that measures the progress of a playing dimension.
  * Additionally it can stop the playing synth when that progress has reached 100%.
  *
  * @param in         the playing dimension
  * @param terminate   if `true`, stops the synth when 100% is reached
  */
final case class Elapsed(rate: Rate, in: Dim.Play, terminate: Boolean)
  extends GE.Lazy with SonificationElement with HasSideEffect {

  /* Important: in order for `Dim.IndexRange` to be found in the aural
   * sonification, the "expansion" should happen immediately, not late.
   * Thus it must be created now.
   */
  private val numFrames = in.dim.size

  protected def makeUGens: UGenInLike = {
    val b         = UGenGraphBuilder.get
    val key: String = ??? //       = AuralSonificationOLD.current().attributeKey(this)
    val reportID  = proc.graph.attribute(key).ir
    import synth._
    import synth.ugen._
    val frame     = Sweep(rate, 0, in.freq) // speed = increment _per second_
    val ratio     = frame / numFrames
    //        in.freq      .poll(0, "freq")
    //        frame        .poll(2, "frame")
    //        numFrames    .poll(0, "numFrames")
    val report    = Impulse(rate, 20)
    val reportSig = Seq(ratio, in): GE
    SendReply(rate, report, reportSig, "/$elpsd", id = reportID)
    if (terminate) {
      val done    = ratio >= 1
      // because the `report` pulse might not fire exactly at 100%,
      // and the synth goes to pause at 100%, we must unfortunately
      // create a second `SendReply` for that singular event.
      // (it's also not safe to add the two triggers, because we might
      // have no new trigger at exactly 100%)
      SendReply(rate, done, Seq(ratio, in), "/$elpsd", id = reportID)
      val doneK   = if (rate == audio) A2K.kr(done) else done
      // the aural-sonif will stop the transport. what we must
      // ensure is that the sounds stops immediately. to avoid
      // annoying 'node not found' message, pause instead of free the synth.
      PauseSelf.kr(doneK)
    }
    ratio
  }
}

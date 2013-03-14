package at.iem.sysson

import sound.AudioSystem
import Implicits._
import de.sciss.synth
import synth._
import io.SampleFormat
import ugen._
import synth.Ops._

/** A utility trait that can be mixed in to quickly get going with some tests.
  * For example do `object MySession extends SessionLike` and define the `run` method
  * in `MySession`.
  *
  * This yields an executable object (startable via `sbt test:run`).
  *
  * It takes care of booting scsynth and preparing the audio system for `Sonification`.
  *
  * It also provides a convenient `record` method to pipe the real-time sound synthesis.
  *
  * The default NetCDF data file is bound to variable `f`.
  */
trait SessionLike extends App {
  /** When `true`, the `record` method indeed does record to a sound file. If `false`,
    * it just launches the payload synth and doesn't record anything.
    */
  var rec = false

  /** You must define this method which is invoked when the audio system is ready and the data file has been opened. */
  def run(): Unit

  AudioSystem.start().whenBooted { _ => run() }

  lazy val dataName  = "25_ta_Amon_HadGEM2-ES_rcp45_r1i1p1_200512-210012.nc"
  lazy val dataFile  = dataDir / "201211" / "gcm" / "RCP45" / "MetOffUK_HadGEM2-ES" / dataName

  /** Recordings will be placed inside a directory `rec` in the SysSon base directory. */
  lazy val recDir    = syssonDir / "rec"

  /** The opened default NetCDF file to play around with. */
  lazy val f = openFile(dataFile)

  def s = Server.default

  def quit() {
    AudioSystem.instance.stop()
    sys.exit()
  }

  /** Given a function that creates a synth, this method sets up another recording synth that captures
    * the default stereo output while that synth is running.
    *
    * @param  name  a suffix that will be used in the output sound file name
    * @param  fun   a block of code that creates and launches the synth which is to be recorded
    */
  def record(name: String)(fun: => Synth) {
    if (!rec) {
      fun
      return
    }

    val fRec  = recDir / s"${dataName}_$name.aif"
    val dfRec = SynthDef("_rec") {
      val sig = In.ar(0, 2)
      DiskOut.ar("buf".ir, Limiter.ar(sig, level = -0.2.dbamp))
    }
    val bRec  = Buffer(s)
    val syRec = Synth(s)

    print(s"Recording ${fRec.getName} ...")

    bRec.alloc(numFrames = 32768, numChannels = 2, completion =
      bRec.writeMsg(fRec.getAbsolutePath, sampleFormat = SampleFormat.Int16, numFrames = 0, startFrame = 0,
        leaveOpen = true, completion =
          dfRec.recvMsg(completion =
            syRec.newMsg(dfRec.name, addAction = addToTail, args = Seq("buf" -> bRec.id))
          )
      )
    )

    val sySon = fun
    sySon.onEnd {
      syRec.free()
      bRec.close()
      bRec.free()
      println(" Done.")
    }
  }
}
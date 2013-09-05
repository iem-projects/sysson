package at.iem.sysson

import sound.{MatrixIn, Sonification, AudioSystem}
import de.sciss.synth
import de.sciss.synth.{SynthGraph, Synth, Server}
import Implicits._
import concurrent.{ExecutionContext, duration, future}
import duration._
import de.sciss.osc
import ExecutionContext.Implicits.global

object SonifTest extends App {
  val mode      = "matrix"    // either of "row", "column", and "matrix"

  val as        = AudioSystem.start()

  val eachDur     = if (mode != "column") 10.0 else 3.0
  val iterations  = if (mode == "column") 10 else 0

  val son   = Sonification("test")
  son.patch = Patch("Test", SynthGraph {
    import synth._
    import ugen._
    val data  = MatrixIn.ar("vec")
    val clip  = data.max(0).min(1)
    val scale = clip.linexp(0, 1, 100, 10000)
    val sin   = SinOsc.ar(scale) * data.numRows.reciprocal.sqrt
    val sig   = Pan2.ar(Mix(sin))
    WrapOut(sig)
  })
  // son.matrices += "vec" -> MatrixSpec()

  val f     = openDefault()
  val v     = f.variableMap("refr")
  val sec0  = v in "plev" select 0
  val sec1  = sec0.normalized

  def playLat(lat: Int): Synth = {

    val source = if (mode == "matrix") {
      sec1.asMatrix(row = "time", column = "lat") // ("lat", "time")
    } else {
      val sec2 = sec1 in "lat" select lat
      if (mode == "column") {
        println("Lat " + lat)
        sec2.asColumn
      } else {
        sec2.asRow
      }
    }
    son.mapping += "vec" -> source
    ??? : Synth // son playOver eachDur.seconds
  }

  def play(s: Server) {
    s.dumpOSC(osc.Dump.Text)

    def loop(lat: Int) {
      playLat(lat)
      future {
        Thread.sleep((eachDur * 1000).toLong)
      } onSuccess { case _ =>
        if (lat + 1 < iterations) loop(lat + 1) else {
          println("Quitting...")
          as.stop()
          sys.exit(0)
        }
      }
    }

    loop(0)
  }

  as.whenBooted(play)
}
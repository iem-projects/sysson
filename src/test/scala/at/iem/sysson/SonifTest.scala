package at.iem.sysson

import sound.{MatrixSpec, MatrixIn, Sonification, AudioSystem}
import de.sciss.synth
import synth.{Synth, Server}
import Implicits._
import concurrent.{ExecutionContext, duration, future}
import duration._
import de.sciss.osc
import ExecutionContext.Implicits.global

object SonifTest extends App {
  val asRow     = true  // if false: uses asColumn

  val cfg       = Server.Config()
  cfg.transport = osc.TCP
  val as        = AudioSystem.instance.start(cfg)

  val eachDur     = if (asRow) 10.0 else 3.0
  val iterations  = if (asRow) 0 else 10

  val son   = Sonification("test")
  son.graph = {
    import synth._
    import ugen._
    val data  = MatrixIn.ar("vec")
    val clip  = data.max(0).min(1)
    val scale = clip.linexp(0, 1, 100, 10000)
    val sin   = SinOsc.ar(scale) * 0.09
    Pan2.ar(Mix(sin))
  }
  son.matrices += "vec" -> MatrixSpec()

  val f     = openDefault()
  val v     = f.variableMap("refr")
  val sec0  = v in "plev" select 0
  val sec1  = sec0.normalized

//  son._debug_writeDef()

  def playLat(lat: Int): Synth = {
    val sec2  = sec1 in "lat" select lat

    son.mapping += "vec" -> (if (asRow) sec2.asRow else sec2.asColumn)
    println("Lat " + lat)
    if (asRow) {
      son play(5)
    } else {
      son playOver eachDur.seconds
    }
  }

  def play(s: Server) {
    s.dumpOSC(osc.Dump.Text)

    def loop(lat: Int) {
      /* val synth = */ playLat(lat)
      future {
        Thread.sleep((eachDur * 1000).toLong)
//        synth.free()
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

  as.whenBooted(play _)
}
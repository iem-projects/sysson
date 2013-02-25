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
  val cfg       = Server.Config()
  cfg.transport = osc.TCP
  val as        = AudioSystem.instance.start(cfg)

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

    son.mapping += "vec" -> sec2.asColumn
    println("Lat " + lat)
    son playOver 3.seconds
  }

  def play(s: Server) {
//    s.dumpOSC(osc.Dump.Text)

    def loop(lat: Int) {
      /* val synth = */ playLat(lat)
      future {
        Thread.sleep(3000)
//        synth.free()
      } onSuccess { case _ =>
        if (lat < 9) loop(lat + 1) else {
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
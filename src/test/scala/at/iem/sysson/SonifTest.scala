package at.iem.sysson

import sound.{MatrixSpec, MatrixIn, Sonification, AudioSystem}
import de.sciss.synth
import synth.{Server, SynthGraph}
import Implicits._
import concurrent.{ExecutionContext, duration, future}
import duration._
import de.sciss.osc.{TCP, Dump}
import ExecutionContext.Implicits.global

object SonifTest extends App {
  val cfg       = Server.Config()
  cfg.transport = TCP
  val as        = AudioSystem.instance.start(cfg)

  val son   = Sonification("test")
  son.graph = SynthGraph {
    import synth._
    import ugen._
    val data  = MatrixIn.ar("vec")
    val sin   = SinOsc.ar(data.max(0).min(1).linexp(0, 1, 100, 10000)) * 0.09
    Out.ar(0, Pan2.ar(Mix(sin)))
  }
  son.matrices += "vec" -> MatrixSpec()

  val f     = openDefault()
  val v     = f.variableMap("refr")
  val sec1  = v    in "plev" select 0
  val sec2  = sec1 in "lat"  select 0
  val sec3  = sec2 in "time" select (0 to 10) // XXX test

  son.mapping += "vec" -> sec3.asColumn

  def play(s: Server) {
    s.dumpOSC(Dump.Text)
    son play 1.0
    future {
      Thread.sleep(10000)
      println("Quitting...")
      sys.exit(0)
    }
  }

  as.whenBooted(play _)
}
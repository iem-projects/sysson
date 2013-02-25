package at.iem.sysson

import sound.{MatrixSpec, MatrixIn, Sonification, AudioSystem}
import de.sciss.synth
import synth.{Server, SynthGraph}
import Implicits._
import concurrent.{ExecutionContext, duration, future}
import duration._
import de.sciss.osc.Dump
import ExecutionContext.Implicits.global

object SonifTest extends App {
  val as = AudioSystem.instance.start()

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

  son.mapping += "vec" -> sec2.asColumn

  def play(s: Server) {
    s.dumpOSC(Dump.Text)
    son play 1.0
    future { Thread.sleep(1000); println("AQUI"); sys.exit(0) }
  }

  as.whenBooted(play _)
}
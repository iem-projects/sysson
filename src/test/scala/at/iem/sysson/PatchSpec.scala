package at.iem.sysson

import at.iem.sysson.sound.Patch
import de.sciss.synth.proc.{Obj, SynthGraphs}
import de.sciss.synth.SynthGraph
import de.sciss.synth.impl.DefaultUGenGraphBuilderFactory

/*
 To run only this suite:

 test-only at.iem.sysson.PatchSpec

 */
class PatchSpec extends DurableSpec {
  "A Patch" should "serialize and de-serialize" in { system =>
    val g0 = SynthGraph {
      import de.sciss.synth._
      import ugen._
      val v = LFDNoise3.ar(0.1).linexp(-1, 1, 1e-5, 1)
      val n = 32
      var mix: GE = 0
      for (i <- 0 until n) {
        val freq = LFDNoise3.ar(LFDNoise3.ar(v).linexp(-1, 1, 1e-4, 0.01)).linexp(-1, 1, 64, 16000)
        val sin  = SinOsc.ar(freq)
        val mul  = LFDNoise3.ar(LFDNoise3.ar(v).linexp(-1, 1, 1e-4, 1.00)).linexp(-1, 1, 0.001, 1)
        mix += sin * mul
      }
      val sig = OnePole.ar(mix / n * 2, 0.95)
      Out.ar(0, Pan2.ar(sig))
    }

    val poH = system.step { implicit tx =>
      val p     = Patch[S]
      p.graph() = SynthGraphs.newConst[S](g0)
      val obj   = Obj(Patch.Elem(p))
      tx.newHandle(obj)
    }

    system.step { implicit tx =>
      val p   = poH().elem.peer
      val g1  = p.graph.value
      assert(g1 === g0)
    }
  }
}
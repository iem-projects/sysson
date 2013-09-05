package at.iem.sysson

import de.sciss.synth.{SynthGraph, ugen}

object Library {
  sealed trait Node { def name: String }
  case class Branch(name: String, children: Vec[Node]) extends Node
  case class Child(patch: Patch) extends Node {
    def name = patch.name
  }

  // sealed trait Update
}
trait Library /* extends Model[Library.Update] */ {
  def root: Library.Branch
}

object TestLibrary extends Library {
  import Library.{Child, Branch}
  import ugen._
  import graph._

  val root = Branch("root",
    Vec(
      Child(Patch("Test-Static-Range", SynthGraph {
        val plevRange = SelectedRange(Pressure)
        plevRange.values.poll(Impulse.kr(0), label = "plev")
      })),

      //      Child(Patch("Test-Sonif", SynthGraph {
      //        val latRange  = SelectedRange(Latitude)
      //        val timeRange = SelectedRange(Time)
      //        val plev      = SelectedValue(Pressure)
      //        // val speed     = RotaryKnob(speedSpec)   // --> position, label etc. via view-map ?
      //
      //        val sel       = Var().select(latRange, timeRange, plev).average(Longitude)
      //        val freq      = 1.0 // speed.kr
      //        val time      = timeRange.play(freq)
      //        val sig       = sel.play(time)
      //        // val sig       = WhiteNoise.ar // sel.ar(time)
      //
      //        Pan2.ar(SinOsc.ar(sig), sig.axisValues(Latitude).linlin(latRange.startValue, latRange.endValue, -1, 1))
      //      })),

      Child(Patch("With-Altitude", SynthGraph {
        import graph._
        SelectedRange(Altitude)
      }))
    )
  )
}
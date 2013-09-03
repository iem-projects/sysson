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
  val root = Library.Branch("root",
    Vec(
      Library.Child(Patch("Test-Sonif", SynthGraph {
        import ugen._
        import graph._
        val latRange  = SelectedRange(Latitude)
        val timeRange = SelectedRange(Time)
        val plev      = SelectedValue(Pressure)
        // val speed     = RotaryKnob(speedSpec)   // --> position, label etc. via view-map ?

        val sel       = Var().select(latRange, timeRange, plev).average(Longitude)
        val freq      = 1.0 // speed.kr
        val time      = timeRange.ar(freq)
        val sig       = sel.ar(time)
        // val sig       = WhiteNoise.ar // sel.ar(time)

        Pan2.ar(SinOsc.ar(sig), sig.axis(Latitude).linlin(latRange.min, latRange.max, -1, 1))
      })),

      Library.Child(Patch("With-Altitude", SynthGraph {
        import graph._
        SelectedRange(Altitude)
      }))
    )
  )
}
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
        val latRange  = UserSelectRange(Latitude)
        val timeRange = UserSelectRange(Time)
        val plev      = UserSelectValue(Pressure)
        // val speed     = UserRotary(speedSpec)   // --> position, label etc. via view-map ?

        // val sel       = Var.select(latRange).select(timeRange).select(plev)
        // val time      = timeRange.ar(freq)
        val sig       = WhiteNoise.ar // sel.ar(time)

        // Pan2.ar(SinOsc.ar(sig), sig(Latitude).linlin(latRange.min, latRange.max, -1, 1))
      })),

      Library.Child(Patch("With-Altitude", SynthGraph {
        import graph._
        UserSelectRange(Altitude)
      }))
    )
  )
}
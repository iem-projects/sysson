package at.iem.sysson

import util.AuralApp
import de.sciss.synth._
import ugen._

object NearestNTest extends AuralApp {
  play {
    WhiteNoise.ar(0.2)
  }
}
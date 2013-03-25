package at.iem.sysson

import collection.immutable.{IndexedSeq => IIdxSeq}
import Implicits._
import de.sciss.synth
import synth._
import ugen._
import synth.Ops._
import concurrent.duration._

object Session130314 extends SessionLike {
  def run() { test1() }

  def test1() {
//    val x = play {
//      val rate = "rate".kr(100)
//      val q = "q".kr(0.1)
//      Resonz.ar(Dust2.ar(Seq(rate,rate)), "freq".kr(500), q.reciprocal) * q
//    }
//    x.set("q" -> 10)
//    x.set("freq" -> 250)
//    x.set("freq" -> 750)
//    x.set("freq" -> 1000)

    val timeIdx = 400
    val v   = f.variableMap("ta")
    val sel = v in "time" select timeIdx

    val numPlev = f.dimensionMap("plev").size
    val numLat  = f.dimensionMap("lat" ).size
    val numLon  = f.dimensionMap("lat" ).size

    sys.error("Unfinished")

//    for (plev <- 0 until numPlev) {
//      val selp    = sel in "plev" select plev
//      val lattice = selp.read().float1D
//      val
//    }
  }
}
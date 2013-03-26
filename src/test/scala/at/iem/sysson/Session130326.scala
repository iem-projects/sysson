package at.iem.sysson

import collection.immutable.{IndexedSeq => IIdxSeq}
import Implicits._
import de.sciss.synth
import synth._
import io.{AudioFileSpec, AudioFile}
import ugen._
import synth.Ops._
import concurrent.duration._
import java.io.File

object Session130326 extends SessionLike {
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
    val plevIdx = 0

    val v   = f.variableMap("ta")

    val numPlev = f.dimensionMap("plev").size
    val numLat  = f.dimensionMap("lat" ).size
    val numLon  = f.dimensionMap("lon" ).size

    val sel = (v in "time" select timeIdx) in "plev" select plevIdx
    val nan = v.fillValue
    val arr = sel.read().float1D.normalize(nan).replaceNaNs(-1, nan)
    println(s"Min ${arr.min}, Max ${arr.max}")

    val rate = 1.0/10

    val tmp = File.createTempFile("sysson", ".aif")
    val af  = AudioFile.openWrite(tmp, AudioFileSpec(numChannels = 1, sampleRate = 44100))
    af.write(Array(arr.toArray))
    af.close()

    val bufSz = if (arr.size.isPowerOfTwo) arr.size else arr.size.nextPowerOfTwo >> 1
    println(s"Buffer size $bufSz")
    require(bufSz >= 64)
    val buf   = Buffer.cue(s, tmp.path, bufFrames = bufSz)

    val x = play {
      val disk = VDiskIn.ar(numChannels = 1, buf = buf.id, speed = rate, loop = 1)
      Line.kr(dur = 10, doneAction = freeSelf)
      Pan2.ar(HPZ1.ar(disk))
    }

    x.onEnd {
      buf.close(); buf.free()
      tmp.delete()
      quit()
    }

//    for (plev <- 0 until numPlev) {
//      val selp    = sel in "plev" select plev
//      val lattice = selp.read().float1D
//      val
//    }
  }
}
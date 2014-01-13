package at.iem.sysson

import Implicits._
import de.sciss.synth._
import ugen._
import Ops._
import scala.concurrent._
import duration._

object Session130423 extends SessionLike {
  rec = true

  def run() {
    println("Run...")
    test1()
  }

  // (60 until 63, 30 until 33, 2 until 5)
  def test1() {
    graphNNN("Cube1")(lonRange = 60 until 64, latRange = 30 until 34, plevRange = 2 until 6) {
      (min, max, d, lon, lat, plev) =>
      val freq  = d  .linexp(min - 20, max, 200, 4000)
      val pan   = lon.linlin(       0,   3,  -1,    1)
      Pan2.ar(SinOsc.ar(freq) * 0.1, pan)
    }
  }

  def sync() {
    val msg = s.syncMsg()
    val id  = msg.id
    val fut = s.!!(msg) {
      case message.Synced(`id`) => true
    }
    Await.ready(fut, Duration.Inf)
  }

  // helper method
  def graphNNN(name: String)(lonRange: Range, latRange: Range, plevRange: Range)
              (fun: (Double, Double, GE, Int, Int, Int) => GE) {
    val v     = f.variableMap("ta")
    val sel1  = v    in "lon"  select lonRange
    val sel2  = sel1 in "lat"  select latRange
    val sel2b = sel2 in "plev" select plevRange

    val sel3  = sel2b // in "time" select (0 until 250)

    // val (min, max) = if (useGivenMinMax) (givenMin, givenMax) else sel3.minmax
    val stat  = sel3.stats.!!
    val min   = stat.min
    val max   = stat.max

    // val sz1  = sel3.size
    // val shp1 = sel3.shape
    // println()

    val data0     = sel3.readScaled1D()
    val data      = data0.replaceNaNs(value = (min - 20).toFloat, fillValue = v.fillValue)
    val numLat    = latRange.size
    val numLon    = lonRange.size
    val numPlev   = plevRange.size
    val numTime   = f.variableMap("time").size.toInt
    val numLatLon = numLat * numLon
    val numCh     = numLat * numLon * numPlev // sel3.shape.product / numTime
    // assert(numCh == )

    println(f"min = $min%1.2f, max = $max%1.2f, numCh = $numCh, data.size = ${data.size}, numLat = $numLat, numLon = $numLon, numPlev = $numPlev, numTime = $numTime")

    val b     = Buffer()
    b.alloc(numFrames = numTime, numChannels = numCh)
    val chunk = math.max(1, 8192/numCh)
    var sent = 0
    while (sent < numTime) {
      val stop  = math.min(sent + chunk, numTime)
      val off   = sent * numCh
      val sub   = data.slice(off, stop * numCh)
      println(s"Blip... $off + ${sub.size}")
      b.setn(off -> sub)
      sent = stop
      sync()
    }


    // [time][plev][lat][lon]
    // input data
    //     0     0    0    0
    //     0     0    0    1
    //                     numLon-1
    //                1    0
    //           numLat-1  numLon-1
    //...

    val son = sound.SonificationOLD("Test")
    son.patch = Patch.withoutSource("Test", SynthGraph {
      var sum: GE = 0
      val speed = 1.0 / 4000.0
      // dur = numTime / (SampleRate * speed)
      val play = PlayBuf.ar(numChannels = numCh, buf = b.id, speed = speed, loop = 0, doneAction = freeSelf)
      for(lon <- 0 until numLon) {
        for(lat <- 0 until numLat) {
          for(plev <- 0 until numPlev) {
            // val idx = lon * (latRange.size * plevRange.size) + lat * plevRange.size + plev
            val ch  = lon + lat*numLon + plev*numLatLon
            val d   = play \ ch // sound.MatrixIn.ar("data_" + idx)
            // d.poll(trig = 1, label = s"freq$lon$lat$plev")
            sum += fun(min, max, d, lon, lat, plev)
          }
        }
      }
      WrapOut(sum)
    })

    record(name) {
      val synth: Synth = ??? // son.playOver(20.seconds)
      synth.onEnd {
        b.free()
      }
      synth
    }
  }
}
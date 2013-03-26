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
import concurrent.{Await, Future}

object Session130326 extends SessionLike {
  def run() { test1() }

  rec = true

  def test1() {
    val numTime = f.dimensionMap("time").size
    val numPlev = f.dimensionMap("plev").size

    val timeIdxs = Seq.tabulate(10) { i => (i.toDouble / 10 * numTime).toInt }
    val plevIdx = 5
    val rate    = 1.0/3
    val scan    = 'zig  // 'lon, 'lat, or 'zig as first scanning axis

    scanIter(timeIdxs = timeIdxs, plevIdxs = plevIdx :: Nil, rates = rate :: Nil, scan = scan)
  }

  def test2() {
    val numTime = f.dimensionMap("time").size
    val numPlev = f.dimensionMap("plev").size

    val timeIdxs = 0 :: Nil
    val plevIdxs = (0 until numPlev by 2)
    val rate    = 1.0/3
    val scan    = 'lat  // 'lon, 'lat, or 'zig as first scanning axis

    scanIter(timeIdxs = timeIdxs, plevIdxs = plevIdxs, rates = rate :: Nil, scan = scan)
  }

  def test3() {
    val numTime = f.dimensionMap("time").size
    val numPlev = f.dimensionMap("plev").size

    val timeIdxs = 0 :: Nil
    val plevIdxs = 5 :: Nil // (0 until numPlev by 2)
    val rates   = Seq.tabulate(6)(1.0 / _)
    val scan    = 'zig  // 'lon, 'lat, or 'zig as first scanning axis

    scanIter(timeIdxs = timeIdxs, plevIdxs = plevIdxs, rates = rates, scan = scan)
  }

  def test4() {
    val numTime = f.dimensionMap("time").size
    val numPlev = f.dimensionMap("plev").size

    val timeIdxs = 0 :: Nil
    val plevIdxs = (0 until numPlev by 2)
    val rate    = 1.0/3
    val scan    = 'zig  // 'lon, 'lat, or 'zig as first scanning axis

    scanIter(timeIdxs = timeIdxs, plevIdxs = plevIdxs, rates = rate :: Nil, scan = scan)
  }

  def scanIter(timeIdxs: Seq[Int], plevIdxs: Seq[Int], rates: Seq[Double], scan: Symbol) {
    val dummy   = play { DC.ar(0) }
    record(f"Scan_time_${timeIdxs.mkString("_")}_plev_${plevIdxs.mkString("_")}_rate_${rates.map(r => f"$r%1.2f").mkString("_")}_scan_${scan.name}")(dummy)
    Thread.sleep(100)

    for (timeIdx <- timeIdxs) {
      for (plevIdx <- plevIdxs) {
        for (rate <- rates) {
          val iter = math.ceil(12 * rate).toInt
          println(f"time $timeIdx, plev $plevIdx, rate $rate%1.2f")
          val fut = scanIter1(plevIdx = plevIdx, timeIdx = timeIdx, scan = scan, iter = iter, rate = rate)
          Await.result(fut, 1000.seconds)
        }
      }
    }

    dummy.free()  // and hence record
    quit()
  }

  private def scanIter1(plevIdx: Int, timeIdx: Int, scan: Symbol, iter: Int, rate: Double): Future[Unit] = {
    val nanRplc = 1f  // value to replace NaNs with

    val v       = f.variableMap("ta")
    val numPlev = f.dimensionMap("plev").size
    val numLat  = f.dimensionMap("lat" ).size
    val numLon  = f.dimensionMap("lon" ).size
    val numTime = f.dimensionMap("time").size

    require(plevIdx >=0 && plevIdx < numPlev)
    require(timeIdx >= 0 && timeIdx <= numTime)

    val sel = (v in "time" select timeIdx) in "plev" select plevIdx
    val nan = v.fillValue
    val arr = sel.read().float1D.normalize(nan).replaceNaNs(value = nanRplc, fillValue = nan)
//    println(s"Min ${arr.min}, Max ${arr.max}")

    val tmp   = File.createTempFile("sysson", ".aif")
    val af    = AudioFile.openWrite(tmp, AudioFileSpec(numChannels = 1, sampleRate = 44100))
    val arr1  = new Array[Float](arr.size)

    var ilat = 0; while(ilat < numLat) {
      var ilon = 0; while(ilon < numLon) {
        // val z = arr(y * width + x)
        val z = arr(ilat * numLon + ilon)
        // data.setZValue(x, y, z)
        // data.update(y, x, z)
        if (scan == 'lon) {
          arr1(ilon + ilat * numLon) = z
        } else if (scan == 'zig) {
          if (ilon % 2 == 0) {
            arr1(ilat + ilon * numLat) = z
          } else {
            arr1(numLat - 1 - ilat + ilon * numLat) = z
          }
        } else if (scan == 'lat) {
          if (ilon % 2 == 0) {
            arr1(ilat + (ilon >> 1) * numLat) = z
          } else {
            arr1(numLat - 1 - ilat + ((ilon >> 1) + numLon >> 1) * numLat) = z
          }
        } else {
          sys.error(s"Unknown scanning direction $scan")
        }
      ilon += 1 }
    ilat += 1 }

    af.write(Array(arr1))
    af.close()

    val bufSz = if (arr.size.isPowerOfTwo) arr.size else arr.size.nextPowerOfTwo >> 1
//    println(s"Buffer size $bufSz")
    require(bufSz >= 64)
    val buf   = Buffer.cue(s, tmp.path, bufFrames = bufSz)

    val dur   = arr.size.toDouble / (44100 * rate) * iter

    val x = play {
      val disk0 = VDiskIn.ar(numChannels = 1, buf = buf.id, speed = rate, loop = 1)
      val disk1 = HPZ1.ar(disk0)
      val disk  = LPF.ar(disk1, SampleRate.ir * rate * 0.5)
      Line.kr(dur = dur, doneAction = freeSelf)
      Pan2.ar(disk * 2 * ToggleFF.kr(DelayN.kr(Impulse.kr(0), ControlDur.ir, ControlDur.ir)))
    }

    val prom = concurrent.promise[Unit]()

    x.onEnd {
      buf.close(); buf.free()
      tmp.delete()
      // quit()
      prom.success(())
    }

    prom.future

//    for (plev <- 0 until numPlev) {
//      val selp    = sel in "plev" select plev
//      val lattice = selp.read().float1D
//      val
//    }
  }
}
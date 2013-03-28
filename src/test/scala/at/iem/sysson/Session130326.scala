package at.iem.sysson

import collection.immutable.{IndexedSeq => IIdxSeq}
import Implicits._
import de.sciss.{osc, synth}
import synth._
import io.{AudioFileSpec, AudioFile}
import ugen._
import synth.Ops._
import concurrent.duration._
import java.io.File
import concurrent.{Await, Future}

object Session130326 extends SessionLike {
  def run() { test5() }

  rec = true

  val minTA = 167.23929f
  val maxTA = 301.96228f

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

  def test5() {
    val numTime = f.dimensionMap("time").size
    val numPlev = f.dimensionMap("plev").size

    val timeIdxs = (0 until (numTime/4) by 30)
    val plevIdxs = 5 :: Nil // (0 until numPlev)
    val rates    = 1.0/3 :: Nil
    val scan     = 'lat  // 'lon, 'lat, or 'zig as first scanning axis

    scanIter(timeIdxs = timeIdxs, plevIdxs = plevIdxs, rates = rates, scan = scan, iter = Some(1))
  }

  def scanIter(timeIdxs: Seq[Int], plevIdxs: Seq[Int], rates: Seq[Double], scan: Symbol, iter: Option[Int] = None) {
    val dummy   = play { DC.ar(0) }
    record(f"Scan_time_${timeIdxs.mkString("_")}_plev_${plevIdxs.mkString("_")}_rate_${rates.map(r => f"$r%1.2f").mkString("_")}_scan_${scan.name}")(dummy)
    Thread.sleep(100)

    for (timeIdx <- timeIdxs) {
      for (plevIdx <- plevIdxs) {
        for (rate <- rates) {
          val iter0 = iter getOrElse math.ceil(12 * rate).toInt
          println(f"time $timeIdx, plev $plevIdx, rate $rate%1.2f")
          val fut = scanIter1(plevIdx = plevIdx, timeIdx = timeIdx, scan = scan, iter = iter0, rate = rate)
          Await.result(fut, 1000.seconds)
        }
      }
    }

    dummy.free()  // and hence record
    quit()
  }

  private def scanIter1(plevIdx: Int, timeIdx: Int, scan: Symbol, iter: Int, rate: Double, min: Float = minTA, max: Float = maxTA): Future[Unit] = {
    val nanRplc = max // value to replace NaNs with

    val v       = f.variableMap("ta")
    val numPlev = f.dimensionMap("plev").size
    val numLat  = f.dimensionMap("lat" ).size
    val numLon  = f.dimensionMap("lon" ).size
    val numTime = f.dimensionMap("time").size

    require(plevIdx >=0 && plevIdx < numPlev)
    require(timeIdx >= 0 && timeIdx <= numTime)

    val sel = (v in "time" select timeIdx) in "plev" select plevIdx
    val nan = v.fillValue
//    val in  = sel.read().float1D.normalize(nan).replaceNaNs(value = nanRplc, fillValue = nan)
    val in  = sel.read().float1D.replaceNaNs(value = nanRplc, fillValue = nan).linlin(min, max)(0.0f, 1.0f)

    val tmp   = File.createTempFile("sysson", ".aif")
    val af    = AudioFile.openWrite(tmp, AudioFileSpec(numChannels = 1, sampleRate = 44100))
    val out   = new Array[Float](in.size)

    var ilat = 0; while(ilat < numLat) {
      var ilon = 0; while(ilon < numLon) {
        val idx = ilat * numLon + ilon

        if (scan == 'lon) {
          val z = in(ilat * numLon + ilon)
          out(ilon + ilat * numLon) = z
        } else if (scan == 'zig) {
          val z = in(ilat * numLon + ilon)
          if (ilon % 2 == 0) {
            out(ilat + ilon * numLat) = z
          } else {
            out(numLat - 1 - ilat + ilon * numLat) = z
          }
        } else if (scan == 'lat) {
          val colIdx = idx / numLat
          val rowIdx = idx % numLat
          // abwechselnd die folgenden zwei faelle
          val z = if (colIdx % 2 == 0) {
            // bei gerader scanner spalte:
            // longitude = scanner spalte / 2
            // latitude  = scanner index modulus numLat
            val readLon = colIdx / 2
            val readLat = rowIdx
            in(readLat * numLon + readLon)
          } else {
            // bei ungerader scanner spalte:
            // longitude = scanner spalte / 2 + numLon/2
            // latitude  = scanner index modulus numLat
            val readLon = (colIdx + numLon) / 2
            val readLat = numLat - 1 - rowIdx
            in(readLat * numLon + readLon)
          }
          out(idx) = z

        } else {
          sys.error(s"Unknown scanning direction $scan")
        }
      ilon += 1 }
    ilat += 1 }

    af.write(Array(out))
    af.close()

    val bufSz = if (in.size.isPowerOfTwo) in.size else in.size.nextPowerOfTwo >> 1
//    println(s"Buffer size $bufSz")
    require(bufSz >= 64)

    val buf = Buffer(s)

    val sr    = 44100 * rate
    val dur   = in.size.toDouble / sr * iter

    val df = SynthDef("sysson") {
      val disk0 = VDiskIn.ar(numChannels = 1, buf = buf.id, speed = rate, loop = 1)
      val disk1 = HPZ1.ar(disk0)
//      val disk  = disk1 // LPF.ar(disk1, SampleRate.ir * rate * 0.5)
      val baseFreq:GE = sr / (if (scan == 'lon) numLon else numLat)
//      baseFreq.poll(0)
      val disk  = BRF.ar(disk1, freq = baseFreq, rq = 1)
      val env0  = EnvGen.ar(Env.linen(0.02, dur - 0.04, 0.02), doneAction = freeSelf)
      val env   = DelayN.ar(env0, ControlDur.ir * 2, ControlDur.ir * 2)
      val sig   = Pan2.ar(disk * 8 * env) // Lag.kr(ToggleFF.kr(DelayN.kr(Impulse.kr(0), ControlDur.ir, ControlDur.ir))))
      WrapOut(sig, fadeTime = None)
    }

    val x = Synth(s)

//    Buffer.cue(s, tmp.path, bufFrames = bufSz)
    buf.alloc(numFrames = bufSz, numChannels = 1,
      buf.cueMsg(tmp.path, startFrame = 0, df.recvMsg(x.newMsg(df.name)))
    )

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
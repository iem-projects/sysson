package at.iem.sysson

import Implicits._
import de.sciss.{osc, synth}
import synth._
import ugen._
import synth.Ops._
import concurrent.duration._
import concurrent.{Await, Future}

object Session130326 extends SessionLike {
  def run(): Unit = test6()

  rec = true

  lazy val minTA  = 167.23929f
  lazy val maxTA  = 301.96228f
  var AMP    = 16
  var FM     = false
  var INTERP = true
  var MOD    = true

  def test1(): Unit = {
    val numTime = f.dimensionMap("time").size
    val numPlev = f.dimensionMap("plev").size

    val timeIdxs = Seq.tabulate(10) { i => (i.toDouble / 10 * numTime).toInt }
    val plevIdx = 5
    val rate    = 1.0/3
    val scan    = 'zig  // 'lon, 'lat, or 'zig as first scanning axis

    scanIter(timeIdxs = timeIdxs, plevIdxs = plevIdx :: Nil, rates = rate :: Nil, scan = scan)
  }

  def test2(): Unit = {
    val numTime = f.dimensionMap("time").size
    val numPlev = f.dimensionMap("plev").size

    val timeIdxs = 0 :: Nil
    val plevIdxs = 0 until numPlev by 2
    val rate    = 1.0/3
    val scan    = 'lat  // 'lon, 'lat, or 'zig as first scanning axis

    scanIter(timeIdxs = timeIdxs, plevIdxs = plevIdxs, rates = rate :: Nil, scan = scan)
  }

  def test3(): Unit = {
    val numTime = f.dimensionMap("time").size
    val numPlev = f.dimensionMap("plev").size

    val timeIdxs = 0 :: Nil
    val plevIdxs = 5 :: Nil // (0 until numPlev by 2)
    val rates   = (1 to 6).map(1.0 / _)
    val scan    = 'zig  // 'lon, 'lat, or 'zig as first scanning axis

    scanIter(timeIdxs = timeIdxs, plevIdxs = plevIdxs, rates = rates, scan = scan)
  }

  def test4(): Unit = {
    val numTime = f.dimensionMap("time").size
    val numPlev = f.dimensionMap("plev").size

    val timeIdxs = 0 :: Nil
    val plevIdxs = 0 until numPlev by 2
    val rate    = 1.0/3
    val scan    = 'zig  // 'lon, 'lat, or 'zig as first scanning axis

    scanIter(timeIdxs = timeIdxs, plevIdxs = plevIdxs, rates = rate :: Nil, scan = scan)
  }

  // session mit kathi
  def test5(): Unit = {
    val numTime = f.dimensionMap("time").size

    FM      = false
    INTERP  = true
    MOD     = false
    AMP     = 32

//    val timeIdxs = (0 until (numTime/10) by 3)
    val timeIdxs = 0 until numTime/30
    val plevIdxs = 5 :: Nil // (0 until numPlev)
    val rates    = 1.0/3 :: Nil
    val scan     = 'lat  // 'lon, 'lat, or 'zig as first scanning axis

    scanIter(timeIdxs = timeIdxs, plevIdxs = plevIdxs, rates = rates, scan = scan, iter = Some(1))
  }

  // fm (delay modulation) versuche. noch nicht gut.
  def test6(): Unit = {
    val numTime = f.dimensionMap("time").size

    FM      = false
    INTERP  = true
    MOD     = true
    AMP     = 128

    val timeIdxs = 0 until numTime/90
    val plevIdxs = 5 :: Nil // (0 until numPlev)
    val rates    = 1.0/15 :: Nil
    val scan     = 'lat  // 'lon, 'lat, or 'zig as first scanning axis

    scanIter(timeIdxs = timeIdxs, plevIdxs = plevIdxs, rates = rates, scan = scan, iter = Some(1))
  }

  private def mkString(t: Seq[_]): String = {
    val sz = t.size
    if (sz <= 6) t.mkString("_")
    else t.take(3).mkString("_") + "_()_" + t.takeRight(3).mkString("_")
  }

  def scanIter(timeIdxs: Seq[Int], plevIdxs: Seq[Int], rates: Seq[Double], scan: Symbol,
               iter: Option[Int] = None): Unit = {
    val dummy   = play { DC.ar(0) }
    record(f"Scan_time_${mkString(timeIdxs)}_plev_${mkString(plevIdxs)}_rate_${mkString(rates.map(r => f"$r%1.2f"))}_scan_${scan.name}${if (FM) "FM" else ""}")(dummy)
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

    Thread.sleep(500)
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
    val in  = sel.read().double1D.replaceNaNs(value = nanRplc, fillValue = nan).linlin(min, max)(0.0f, 1.0f)

//    val tmp   = File.createTempFile("sysson", ".aif")
//    val af    = AudioFile.openWrite(tmp, AudioFileSpec(numChannels = 1, sampleRate = 44100))
    val out   = new Array[Float](in.size)

    var ilat = 0; while(ilat < numLat) {
      var ilon = 0; while(ilon < numLon) {
        val idx     = ilat * numLon + ilon
        val colIdx  = idx / numLat
        val rowIdx  = idx % numLat

        val z = if (scan == 'lon) {
          in(ilon + ilat * numLon)

        } else if (scan == 'zig) {
          val readLon = colIdx
          val readLat = if (colIdx % 2 == 0) rowIdx else numLat - 1 - rowIdx
          in(readLat * numLon + readLon)

        } else if (scan == 'lat) {
          // abwechselnd die folgenden zwei faelle
          if (colIdx % 2 == 0) {
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

        } else {
          sys.error(s"Unknown scanning direction $scan")
        }
        out(idx) = z.toFloat

      ilon += 1 }
    ilat += 1 }

//    af.write(Array(out))
//    af.close()

    val bufSz = in.size // if (in.size.isPowerOfTwo) in.size else in.size.nextPowerOfTwo >> 1
//    println(s"Buffer size $bufSz")
//    require(bufSz >= 64)

    val buf = Buffer(s)

    val sr    = 44100 * rate
    val dur   = in.size.toDouble / sr * iter

    val df = SynthDef("sysson") {
//      val disk0 = VDiskIn.ar(numChannels = 1, buf = buf.id, speed = rate, loop = 1)

      val disk0 = if (INTERP) {
        PlayBuf.ar(numChannels = 1, buf = buf.id, speed = rate, loop = 1)
      } else {
        val bufFrames = BufFrames.ir(buf.id)
        val phasor0 = Phasor.ar(speed = rate, lo = 0, hi = bufFrames)
        val phasor  = if (FM) {
          val gridSize  = if (scan == 'lon) numLon else numLat
          val down      = 8
          val fmDepth   = gridSize.toDouble / down // 0.25 // 1.0 // 0.5
//          val noise     = SinOsc.ar(SampleRate.ir/(gridSize * down)) // BrownNoise.ar // WhiteNoise.ar
          val noise     = LFNoise2.ar(sr/down)
          val fmMod     = noise.linlin(-1, 1, 0, fmDepth)
          (phasor0 + fmMod) % bufFrames
        } else phasor0
        BufRd.ar(numChannels = 1, buf = buf.id, index = phasor, loop = 1, interp = 1)
      }

      val disk1  = HPZ1.ar(disk0)

      val disk = if (MOD) {
        PinkNoise.ar(disk1)
      } else disk1

//      val disk  = disk1 // LPF.ar(disk1, SampleRate.ir * rate * 0.5)
//      val baseFreq  = sr / (if (scan == 'lon) numLon else numLat)
//      (baseFreq: GE).poll(0)
      val sig  =
//        if (FM) {
//        val gridSize  = if (scan == 'lon) numLon else numLat
//        val fmDepth   = gridSize * 0.25 // 1.0 // 0.5
////        println(s"fmDepth $fmDepth, gridSize $gridSize")
////        val noise     = LFNoise1.ar(SampleRate.ir/(2 * gridSize))
//        val noise     = WhiteNoise.ar
//        val fmMod     = noise.linlin(-1, 1, 0, fmDepth)
//        DelayC.ar(disk, maxDelayTime = fmDepth / SampleRate.ir, delayTime = fmMod / SampleRate.ir)
//      } else
        disk  // BRF.ar(disk1, freq = baseFreq, rq = 1)

      val env0  = EnvGen.ar(Env.linen(0.02, dur - 0.04, 0.02), doneAction = freeSelf)
      val env   = env0 // DelayN.ar(env0, ControlDur.ir * 2, ControlDur.ir * 2)
      val pan   = Pan2.ar(sig * AMP * env) // Lag.kr(ToggleFF.kr(DelayN.kr(Impulse.kr(0), ControlDur.ir, ControlDur.ir))))
      WrapOut(pan, fadeTime = -1)
    }

    val x = Synth(s)

//    Buffer.cue(s, tmp.path, bufFrames = bufSz)
    buf.alloc(numFrames = bufSz, numChannels = 1,
//      buf.cueMsg(tmp.path, startFrame = 0, df.recvMsg(x.newMsg(df.name)))
      osc.Bundle.now(
        buf.setnMsg(out.toVector),
        df.recvMsg(x.newMsg(df.name))
      )
    )

    val prom = concurrent.Promise[Unit]()

    x.onEnd {
      buf.close(); buf.free()
//      tmp.delete()
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
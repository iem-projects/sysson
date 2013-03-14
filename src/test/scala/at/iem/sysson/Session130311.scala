package at.iem.sysson

import sound.AudioSystem
import Implicits._
import de.sciss.synth
import synth._
import io.SampleFormat
import ugen._
import synth.Ops._
import concurrent.duration._

object Session130311 extends App {
  AudioSystem.start().whenBooted { _ =>
    test6()
  }
  lazy val syssonDir = file(sys.props("user.home")) / "Desktop" / "IEM" / "SysSon"
  lazy val dataDir   = syssonDir / "netcdf" / "data"
  lazy val dataName  = "25_ta_Amon_HadGEM2-ES_rcp45_r1i1p1_200512-210012.nc"
  lazy val dataFile  = dataDir / "201211" / "gcm" / "RCP45" / "MetOffUK_HadGEM2-ES" / dataName
  lazy val recDir    = syssonDir / "rec"

  lazy val f = openFile(dataFile)

  def s = Server.default

  def record(name: String)(fun: => Synth) {
    val fRec  = recDir / s"${dataName}_$name.aif"
    val dfRec = SynthDef("_rec") {
      val sig = In.ar(0, 2)
      DiskOut.ar("buf".ir, Limiter.ar(sig, level = -0.2.dbamp))
    }
    val bRec  = Buffer(s)
    val syRec = Synth(s)

    print(s"Recording ${fRec.getName} ...")

    bRec.alloc(numFrames = 32768, numChannels = 2, completion =
      bRec.writeMsg(fRec.getAbsolutePath, sampleFormat = SampleFormat.Int16, numFrames = 0, startFrame = 0,
        leaveOpen = true, completion =
          dfRec.recvMsg(completion =
            syRec.newMsg(dfRec.name, addAction = addToTail, args = Seq("buf" -> bRec.id))
          )
      )
    )

    val sySon = fun
    sySon.onEnd {
      syRec.free()
      bRec.close()
      bRec.free()
      println(" Done.")
    }
  }

  def test1() {
    val v   = f.variableMap("ta")
    val t   = v in "time" select 700
    val sel = ((t in "lat" select 0) in "lon" select 0)
    val h   = sel.read().float1D
    val h1  = h.drop(4)

    val b   = Buffer(s)
    b.alloc(numFrames = h1.size, completion = b.setnMsg(h1))

    play {
      val rate = MouseX.kr(0,1).linexp(0,1,1.0/10000,1.0/100)
      val p = PlayBuf.ar(1, b.id, rate, loop = 1)
      val r = PinkNoise.ar
      val sig = SinOsc.ar(p.linexp(190,220,400,600))
      val isNaN = p > 1e10
      val out = r * isNaN + sig * (1 - isNaN)
      out * 0.1
    }
  }

  def test2() {
    val v   = f.variableMap("ta")
    val t   = v in "time" select 700

    val d = Vector.tabulate(10) { i =>
       val sel = ((t in "lat" select i) in "lon" select 0)
       val h = sel.read().float1D.drop(4)
       h
    }

    val df = Vector.tabulate(13) { f =>
       d.map { v => v(f)}
    }

    val dff = df.flatten
    val b = Buffer(s)
    b.alloc(numFrames = 13, numChannels = 10, completion = b.setnMsg(dff))

    play {
      val rate = "freq".kr(1.0/50) // MouseY.kr(0,1).linexp(0,1,1.0/1000,1.0)
      val p = PlayBuf.ar(10, b.id, rate, loop = 1)
    //  val r = PinkNoise.ar
    //  val isNaN = p > 1e10
      val idx = MouseX.kr(0,10)
      val sel = Select.ar(idx, p)
      val sig = sel.linlin(dff.min,dff.max,-1,1)
      sig * 0.1
    }
  }

  def test3() {
    graph333("PanSin") { (min, max) =>
      var sum: GE = 0
      for(lon <- 0 to 2) {
        for(lat <- 0 to 2) {
          for(plev <- 0 to 2) {
            val idx = lon * 9 + lat * 3 + plev
            val d = sound.MatrixIn.ar("data_" + idx)
            val freq = d.linexp(min,max,400,600)
            val pan = lon.linlin(0, 2, -1, 1)
            sum = sum + Pan2.ar(SinOsc.ar(freq), pan)
          }
        }
      }
      sum * 0.1
    }
  }

  def test4() {
    graph333("PanRingz") { (min, max) =>
      val inp = PinkNoise.ar
      var sum: GE = 0
      for(lon <- 0 to 2) {
        for(lat <- 0 to 2) {
          for(plev <- 0 to 2) {
            val idx = lon * 9 + lat * 3 + plev
            val d = sound.MatrixIn.ar("data_" + idx)
            val freq = d.linexp(min,max,400,600)
            val pan = lon.linlin(0, 2, -1, 1)
            val decay = plev.linexp(0, 2, 0.25, 0.75)
            val sig = Ringz.ar(inp, freq, decay)
            val out = Pan2.ar(sig, pan)
            sum = sum + out
          }
        }
      }
      sum * 0.1
    }
  }

  def test5() {
    graph333("PanRingzPulse") { (min, max) =>
      val inp = PinkNoise.ar
      var sum: GE = 0
      for(lon <- 0 to 2) {
        for(lat <- 0 to 2) {
          for(plev <- 0 to 2) {
            val idx = lon * 9 + lat * 3 + plev
            val d = sound.MatrixIn.ar("data_" + idx)
            val freq = d.linexp(min,max,400,600)
            val pan = lon.linlin(0, 2, -1, 1)
    //        val decay = plev.linexp(0, 2, 0.25, 0.75)
    //        val sig = Ringz.ar(inp, freq, decay)

    //       val sin = SinOsc.ar(freq)
             val sin = Ringz.ar(inp, freq, 1)
//            val mod = Pulse.ar(plev.linexp(0, 2, 1, 5))
             val mod = Lag.ar(Pulse.ar(plev.linexp(0, 2, 2, 17)), plev.linexp(0, 2, 0.01, 0.05))
            val sig = sin * mod
            val out = Pan2.ar(sig, pan)
            sum = sum + out
          }
        }
      }
      sum * 0.1
    }
  }

//  def test6() {
//    son.graph = {
//      val inp = PinkNoise.ar
//      var sum: GE = 0
//      for(lon <- 0 to 2) {
//        for(lat <- 0 to 2) {
//          for(plev <- 0 to 8) {
//            val idx = lon * 27 + lat * 9 + plev
//            val d = sound.MatrixIn.ar("data_" + idx)
//            val pan = lon.linlin(0, 2, -1, 1)
//    //        val decay = plev.linexp(0, 2, 0.25, 0.75)
//    //        val sig = Ringz.ar(inp, freq, decay)
//
//    //       val sin = SinOsc.ar(freq)
//
//            val freq = plev.linexp(0, 8, 200, 7887)
//            val amp = d.linlin(min, max, -30, -6).dbamp
//            val inpa = inp * amp
//             val sin = Ringz.ar(inpa, freq, 1)
//    //         val mod = Pulse.ar(plev.linexp(0, 2, 1, 5))
//            val sig = sin // * mod
//            val out = Pan2.ar(sig, pan)
//            sum = sum + out
//          }
//        }
//      }
//      sum * 0.1
//    }
//  }

  def graph333(name: String)(fun: (Double, Double) => GE) {
    graphNNN(name)(60 until 63, 30 until 33, 10 until 13)(fun)
  }

  def graphNNN(name: String)(lonRange: Range, latRange: Range, plevRange: Range)(fun: (Double, Double) => GE) {
    val v     = f.variableMap("ta")
    val sel1  = v    in "lon"  select lonRange
    val sel2  = sel1 in "lat"  select latRange
    val sel3  = sel2 in "plev" select plevRange
    val (min, max) = sel3.minmax

    val son = sound.Sonification("Test")
    son.graph = fun(min, max)

    val numCells = lonRange.size * latRange.size * plevRange.size

    (0 until numCells).foreach { i => son.matrices += ("data_" + i) -> sound.MatrixSpec() }

    for(lon <- lonRange) {
      for(lat <- latRange) {
        for(plev <- plevRange) {
          val idx = (lon - lonRange.start) * 9 + (lat - latRange.start) * 3 + (plev - plevRange.start)
          val sel1 = v    in "lon"  select lon
          val sel2 = sel1 in "lat"  select lat
          val sel3 = sel2 in "plev" select plev
          son.mapping += ("data_" + idx) -> sel3.asRow
        }
      }
    }

    record(name) {
      son.playOver(20.seconds)
    }
  }
}
package at.iem.sysson

import collection.immutable.{IndexedSeq => IIdxSeq}
import Implicits._
import de.sciss.synth
import synth._
import ugen._
import synth.Ops._
import concurrent.duration._

object Session130311 extends SessionLike {
  def run() { test9() }

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
    graph333("PanSin") { (min, max, d, lon, lat, plev) =>
      val freq = d.linexp(min,max,400,600)
      val pan = lon.linlin(0, 2, -1, 1)
      Pan2.ar(SinOsc.ar(freq) * 0.1, pan)
    }
  }

  def test4() {
    graph333("PanRingz") { (min, max, d, lon, lat, plev) =>
      val inp = PinkNoise.ar
      val freq = d.linexp(min,max,400,600)
      val pan = lon.linlin(0, 2, -1, 1)
      val decay = plev.linexp(0, 2, 0.25, 0.75)
      val sig = Ringz.ar(inp, freq, decay)
      val out = Pan2.ar(sig * 0.1, pan)
      out
    }
  }

  def test5() {
    graph333("PanRingzPulse") { (min, max, d, lon, lat, plev) =>
      val inp = PinkNoise.ar
      val freq = d.linexp(min,max,400,600)
      val pan = lon.linlin(0, 2, -1, 1)
      val sin = Ringz.ar(inp, freq, 1)
      val mod = Lag.ar(Pulse.ar(plev.linexp(0, 2, 2, 17)), plev.linexp(0, 2, 0.01, 0.05))
      val sig = sin * mod
      val out = Pan2.ar(sig * 0.1, pan)
      out
    }
  }

  def test6() {
//    s.dumpOSC()

    graphNNN("Ringz339")(60 until 63, 30 until 33, 5 until 14) { (min, max, d, lon, lat, plev) =>
      val inp = PinkNoise.ar
      val pan = lon.linlin(0, 2, -1, 1)
      val freq = plev.linexp(0, 8, 200, 7887)
      val amp = d.linlin(min, max, -30, -6).dbamp
      val inpa = inp * amp
       val sin = Ringz.ar(inpa, freq, 1)
//         val mod = Pulse.ar(plev.linexp(0, 2, 1, 5))
      val sig = sin // * mod
      val out = Pan2.ar(sig * 0.05, pan)
      out
    }
  }

  def test7() {
    val v = f.variableMap("ta")
    val t = v in "time" select 700
    val numChans = f.dimensionMap("lon").size
    assert(numChans == 144)

    val d = Vector.tabulate(numChans) { i =>
      val sel = ((t in "lon" select i) in "plev" select 7)
      val h = sel.read().float1D // .drop(4)
      h
    }

    val numFrames = d(0).size
    assert(numFrames == f.dimensionMap("lat").size)

    val df = Vector.tabulate(numFrames) { f =>
      d.map(_ apply f)
    }

    val dff = df.flatten
    val b = Buffer(s)
    b.alloc(numFrames = numFrames, numChannels = numChans, completion = b.setnMsg(dff))

    record("PlayBufArray") {
      play {
        val rate = "freq".kr(1.0 / 15) // MouseY.kr(0,1).linexp(0,1,1.0/1000,1.0)
        val p = PlayBuf.ar(numChans, b.id, rate, loop = 1)
  //      val idx = MouseX.kr(0, numChans)
        val idx = LFSaw.kr(freq = 1.0/3, iphase = -1).linlin(-1,1,0,numChans)
        val sel = Select.ar(idx, p)
        val sig = sel.linlin(dff.min, dff.max, -1, 1)

        Line.kr(dur = 20, doneAction = freeSelf)

        LeakDC.ar(sig) * "amp".kr(4)
      }
    }

//    x.set("freq" -> 1.0 / 20)
//    x.set("amp" -> 100)
  }

  def test8() {
    graphArray("PlayBufArray2") { (numChans, numPlevs, bufs) =>
      val idx = LFSaw.kr("lfo".kr(1.0/5),-1).linlin(-1,1,0,numChans)
      val brate = "freq".kr(0.25)
      val out = Mix.tabulate(numPlevs) { plev =>
         val rate = brate * plev.linexp(0,numPlevs-1,1.0/4,1.0)
         val p = PlayBuf.ar(numChans, bufs(plev).id, rate, loop = 1)
         val sel = Select.ar(idx, p)
         val sig = sel.linlin(190,240,-1,1)
         LeakDC.ar(sig)
      }
      out * "amp".kr(0.3) * Line.ar(0, 1, 0.05)
    }
  }

  def test9() {
    graphArray("PlayBufArrayRingz") { (numChans, numPlevs, bufs) =>
      val idx = LFSaw.kr("lfo".kr(1.0/3),-1).linlin(-1,1,0,numChans)
      val brate = "freq".kr(1.0/20.0)
      val out = Mix.tabulate(numPlevs) { plev =>
         val rate = brate // * plev.linexp(0,numPlevs-1,1.0/4,1.0)
         val p = PlayBuf.ar(numChans, bufs(plev).id, rate, loop = 1)
         val sel0 = Select.ar(idx, p)
         val sel1 = sel0.linlin(190,240,-1,1)
         val freq = plev.linexp(0, numPlevs-1,300, 3000)
         val sig = Ringz.ar(sel1, freq, "decay".kr(0.5))
         LeakDC.ar(sig)
      }
      out * "amp".kr(0.075)
    }

//    x.set("freq" -> 1.0/30)
//    x.set("lfo" -> 1.0/2)
//    x.set("decay" -> 0.5)
  }

  // helper method
  def graphArray(name: String)(fun: (Int, Int, IIdxSeq[Buffer]) => GE) {
    val v = f.variableMap("ta")
    val t = v in "time" select 700
    val numChans = f.dimensionMap("lon").size
    val plevRange = 4 until 14

    val numFrames = f.dimensionMap("lat").size

    val ds = plevRange.map { plev =>
      val d0 = Vector.tabulate(numChans) { i =>
         val sel = ((t in "lon" select i) in "plev" select plev)
         val h = sel.read().float1D // .drop(4)
         h
      }

      val d = d0.map { dd => dd.map {
        case value if (value > 1e4f) => 200f
        case value => value
      }}

      assert(d(0).size == numFrames)
      val df = Vector.tabulate(numFrames) { f =>
         d.map { v => v(f)}
      }

      val dff = df.flatten
      dff
    }

    val bs = ds.map { d =>
      val b = Buffer(s)
      b.alloc(numFrames = numFrames, numChannels = numChans, completion = b.setnMsg(d))
      b
    }

    record(name) {
      play {
        Line.kr(dur = 20, doneAction = freeSelf)
        fun(numChans, plevRange.size, bs)
      }
    }
  }

  // helper method
  def graph333(name: String)(fun: (Double, Double, GE, Int, Int, Int) => GE) {
    graphNNN(name)(60 until 63, 30 until 33, 10 until 13)(fun)
  }

  // helper method
  def graphNNN(name: String)(lonRange: Range, latRange: Range, plevRange: Range)
              (fun: (Double, Double, GE, Int, Int, Int) => GE) {
    val v     = f.variableMap("ta")
    val sel1  = v    in "lon"  select lonRange
    val sel2  = sel1 in "lat"  select latRange
    val sel3  = sel2 in "plev" select plevRange
    val (min, max) = sel3.minmax

    val son = sound.Sonification("Test")
    son.graph = {
      var sum: GE = 0
      for(lon <- 0 until lonRange.size) {
        for(lat <- 0 until latRange.size) {
          for(plev <- 0 until plevRange.size) {
            val idx = lon * (latRange.size * plevRange.size) + lat * plevRange.size + plev
            val d   = sound.MatrixIn.ar("data_" + idx)
            sum += fun(min, max, d, lon, lat, plev)
          }
        }
      }
      sum
    }

    val numCells = lonRange.size * latRange.size * plevRange.size

    (0 until numCells).foreach { i => son.matrices += ("data_" + i) -> sound.MatrixSpec() }

    for(lon <- 0 until lonRange.size) {
      for(lat <- 0 until latRange.size) {
        for(plev <- 0 until plevRange.size) {
          val idx = lon * (latRange.size * plevRange.size) + lat * plevRange.size + plev
          val sel1 = v    in "lon"  select (lon  + lonRange.start)
          val sel2 = sel1 in "lat"  select (lat  + latRange.start)
          val sel3 = sel2 in "plev" select (plev + plevRange.start)
          son.mapping += ("data_" + idx) -> sel3.asRow
        }
      }
    }

    record(name) {
      son.playOver(20.seconds)
    }
  }
}
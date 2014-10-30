val vr    = Var("blob")
val dt    = Dim(vr, "time")
val speed = UserValue("speed", 1).kr
val time  = dt.play(speed)
val data  = vr.play(time)

val period = speed.reciprocal

val maxSum = 0.3
val maxMax = 5.3e-4

val dbMin = -36.0

val numBlobs  = 8
val numVoices = numBlobs * 2

// (sig, x, y)
def mkVoice(blobID: Int): (GE, GE, GE) = {
  val off   = blobID * 5
  val gate  = data \ (off + 0)
  val x     = data \ (off + 1)
  val y     = data \ (off + 2)
  val sum   = data \ (off + 3)
  val max   = data \ (off + 4)

  val gateTr    = Trig.ar(gate, SampleDur.ir)
  val toggle    = gateTr + TDelay.ar(gateTr, period)
  val ff        = ToggleFF.ar(toggle)

  def mkCoord(in: GE): GE = {
    val dif0    = in absdif Delay1.ar(in)
    val dif     = Latch.ar(dif0, dif0 > 0)
    val slewRate= dif * speed   // makes Slew work like Ramp
    val inR     = Slew.ar(in, slewRate, slewRate)
    Select.ar(ff, Seq(inR, in))
  }
  
  val env   = Env.asr(attack = period, release = period, curve = Curve.linear)
  val eg    = EnvGen.ar(env, gate = gate)
  // maxSum = 0.29875579476356506
  // maxMax = 5.231246468611062E-4
  
  val amp   = sum.linlin(0, maxSum, dbMin, 0).dbamp - dbMin.dbamp
  val sig   = PinkNoise.ar(amp)
  
  (sig, mkCoord(x), mkCoord(y))
}

def calcIndices(xf: GE, yf: GE): ((GE, GE, GE), (GE, GE, GE), (GE, GE, GE)) = {
  val x   = xf.floor
  val u   = xf - x
  val y0  = yf.floor
  val v   = yf - y0
  
  // note, we add two to avoid problems with negative numbers,
  // but have to take care to subtract that later
  val y   = {
    val cond1 = ((y0 % 2) sig_!= (x % 2))
    val cond2 = u + v < 1
    val cond3 = u - v > 0
    val cond  = (cond1 & cond2) | cond3
    y0 + 2 - cond  // if (cond) y0 + 1 else y0 + 2
  }
  
  val yDiv  = (y / 2).floor - 1 // the minus 1 corrects the offset
  val yOdd  = y % 2
  val yEven = 1 - yOdd
  val xOdd  = x % 2
  
  val vx1  = x + yOdd
  val vy1i = yDiv + yOdd
  val vy1  = vy1i * 2 + (vx1 % 2)
  
  val vx2  = x + (yOdd ^ xOdd)
  val vy2i = yDiv + yEven
  val vy2  = vy2i * 2 + (vx2 % 2)
  
  val vx3  = x + yEven
  val vy3i = yDiv + yOdd
  val vy3  = vy3i * 2 + (vx3 % 2)
  
  // cf. https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
  def dist(x: GE, y: GE)(lx1: GE, ly1: GE, lx2: GE, ly2: GE): GE = {
    // |Dy x0 - Dx y0 - x1 y2 + x2 y1| / sqrt(Dx.squared + Dy.squared)
    // - we assume a normalized Dx, Dy, and thus drop the division
    // - indeed we know the height is two and divide by it
  
    val dx = lx2 - lx1
    val dy = ly2 - ly1
    (dy * x - dx * y - lx1 * ly2 + lx2 * ly1).abs / 2
  }
  
  val df = dist(xf, yf) _
  val d1 = df(vx2, vy2, vx3, vy3)
  val d2 = df(vx3, vy3, vx1, vy1)
  val d3 = df(vx1, vy1, vx2, vy2)
  // println(f"d1 = $d1%1.2f, d2 = $d2%1.2f, d3 = $d3%1.2f, sum = ${d1 + d2 + d3}%1.2f")
  val g1 = d1.sqrt
  val g2 = d2.sqrt
  val g3 = d3.sqrt
  
  ((vx1, vy1i, g1), (vx2, vy2i, g2), (vx3, vy3i, g3))
}

val chanBuf = Buffer("chan-map")

import at.iem.sysson.turbulence.Turbulence
import Turbulence.NumChannels

def OutChan(index: GE, add: GE) =
  PanAz.ar(NumChannels, add, pos = index * 2 / NumChannels)

def mkOutChan(in: GE, xi: GE, yi: GE, gain: GE): GE = {
  val idx  = xi * 5 + yi
  val ch   = Index.ar(chanBuf, idx)
  val chOk = ch >= 0
  val amp  = gain * chOk
  val sig  = in * amp
  OutChan(ch, sig)
}

val mix = Mix.tabulate(numVoices) { vci =>
  val (in, xf, yf) = mkVoice(vci)
  val ((vx1, vy1i, g1), (vx2, vy2i, g2), (vx3, vy3i, g3)) =
    calcIndices(xf = xf, yf = yf)
  val sig1 = mkOutChan(in = in, xi = vx1, yi = vy1i, gain = g1)
  val sig2 = mkOutChan(in = in, xi = vx2, yi = vy2i, gain = g2)
  val sig3 = mkOutChan(in = in, xi = vx3, yi = vy3i, gain = g3)
  sig1 + sig2 + sig3
}

// ScanOut(mix)

Turbulence.Channels.zipWithIndex.foreach { case (spk, ch) =>
  Out.ar(spk.toIndex, mix \ ch)
}


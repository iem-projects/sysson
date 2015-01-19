val vr      = Var("geo")
val dimLat  = Dim(vr, "lat")
val dimLon  = Dim(vr, "lon")
val dimTime = Dim(vr, "time")
val speed   = UserValue("speed", 1).kr

val PRINT = false

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

//// channel buffer ////

val chanBuf = Buffer("chan-map")

def chanOut(in: GE, xi: GE, yi: GE, gain: GE): Unit = {
  val idx  = xi * 5 + yi
  val ch   = Index.ar(chanBuf, idx)
  val chOk = ch > 0
  val amp  = gain * chOk
  val sig  = in * amp
  Out.ar((ch - 1).max(0), sig)
}

//// lat-lon buffer ////

val posBuf  = Buffer("latlon-map")

def getPos(latIdx: GE, lonIdx: GE): (GE, GE) = {
  val xIdx = (latIdx * 144 + lonIdx) * 2
  val yIdx = xIdx + 1
  val x    = Index.ar(posBuf, xIdx)
  val y    = Index.ar(posBuf, yIdx)
  (x, y)
}

//// data ////

val time = dimTime.play(speed)
val tas  = vr.play(time)

val lat  = tas.axis(dimLat).values
val lon  = tas.axis(dimLon).values

val latIdx = lat.linlin(-90, 90, 0, 72) // .roundTo(1)
val lonIdx = lon.linlin(-177.5, 180, 0, 143) // .roundTo(1)

val (xf, yf) = getPos(latIdx, lonIdx)

// val xf = DC.ar(0)
// val yf = DC.ar(0)

// val lagTime  = 8
// val slewRate = 0.5
// 
// val xRaw = K2A.ar(MouseX.kr(0, 13, lag = 0))
// val yRaw = K2A.ar(MouseY.kr(9,  0, lag = 0))
// 
// val xf  = Lag.ar(xRaw, time = lagTime)
// val yf  = Lag.ar(yRaw, time = lagTime)

// val xf = Slew.ar(xRaw, slewRate, slewRate)
// val yf = Slew.ar(yRaw, slewRate, slewRate)

val ((vx1, vy1i, g1), (vx2, vy2i, g2), (vx3, vy3i, g3)) = 
  calcIndices(xf = xf, yf = yf)

val trig = 2

if (PRINT) {
  vx1 .poll(trig, "x1")
  vy1i.poll(trig, "y1")
  vx2 .poll(trig, "x2")
  vy2i.poll(trig, "y2")
  vx3 .poll(trig, "x3")
  vy3i.poll(trig, "y3")
}

//// sonif ////

// val freq = tas.linexp(198, 316, 1, 300)
// val amp  = 0.2
// val in   = Dust.ar(freq) * amp

val freq = tas.linexp(198, 316, 3000, 200)
val amp  = 0.01
val in   = SinOsc.ar(freq) * amp

// val rq = 
// val in = Resonz.ar(WhiteNoise.ar(amp), freq, 

chanOut(in = in, xi = vx1, yi = vy1i, gain = g1)
chanOut(in = in, xi = vx2, yi = vy2i, gain = g2)
chanOut(in = in, xi = vx3, yi = vy3i, gain = g3)


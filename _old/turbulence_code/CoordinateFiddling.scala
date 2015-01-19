val phi = (1 + math.sqrt(5)) / 2

val vx = 0.420152426708710003
val vy = 0.078145249402782959
val vz = 0.904082550615019298

type Pt = (Double, Double, Double)

def mag(in: Pt): Double =
  (in._1.squared + in._2.squared + in._3.squared).sqrt

def mul(in: Pt, f: Double): Pt =
  (in._1 * f, in._2 * f, in._3 *f)

def sub(a: Pt, b: Pt): Pt =
  (b._1 - a._1, b._2 - a._2, b._3 - a._3)

val n = mag((0, 1, phi))

assert(mag(mul((0, 1, phi), mag((0, 1, phi)).reciprocal)).absdif(1) < 1.0e-6)

def rotateX(in: Pt, theta: Double): Pt = {
  // 1 0    0
  // 0 cos -sin
  // 0 sin  cos
 
  val (x, y, z) = in
   
  val cos = math.cos(theta)
  val sin = math.sin(theta)
  
  (x, y * cos - z * sin, y * sin + z * cos)
}

def rotateY(in: Pt, theta: Double): Pt = {
  // cos  0 sin
  // 0    1 0
  // -sin 0 cos
 
  val (x, y, z) = in
   
  val cos = math.cos(theta)
  val sin = math.sin(theta)
  
  (x * cos + z * sin, y, -x * sin + z * cos)
}

def rotateZ(in: Pt, theta: Double): Pt = {
  // cos -sin 0
  // sin  cos 0
  // 0    0   1
 
  val (x, y, z) = in
   
  val cos = math.cos(theta)
  val sin = math.sin(theta)
  
  (x * cos - y * sin, x * sin + y * cos, z)
}

val rad30 = 30 * math.Pi / 180

val v0 = (0.0, 1.0/n, phi/n)
val v1 = (1.0/n, phi/n, 0)

val rad26 = 0.5.atan
val rad20 = 20.905 * math.Pi/180


// for (x <- -180 to 180 by 30) {
//   for (y <- -180 to 180 by 30) {
//     for (z <- -180 to 180 by 30) {
//       val a = rotateZ(rotateY(rotateX(v0, x), y), z)
//       val b = rotateY(rotateZ(rotateX(v0, x), z), y)
//       val c = rotateZ(rotateX(rotateY(v0, y), x), z)
//       val d = rotateX(rotateZ(rotateY(v0, y), z), x)
//       val e = rotateY(rotateX(rotateZ(v0, z), x), y)
//       val f = rotateX(rotateY(rotateZ(v0, z), y), x)
//       Seq(a, b, c, d, e, f).foreach(println)
//     }
//   }
// }

// vertices of icosahedron, according to gray

val gray = Vector(
  ( 0.420152426708710003,  0.078145249402782959,  0.904082550615019298),
  ( 0.995009439436241649, -0.091347795276427931,  0.040147175877166645),
  ( 0.518836730327364437,  0.835420380378235850,  0.181331837557262454),
  (-0.414682225320335218,  0.655962405434800777,  0.630675807891475371),
  (-0.515455959944041808, -0.381716898287133011,  0.767200992517747538),
  ( 0.355781402532944713, -0.843580002466178147,  0.402234226602925571),
  ( 0.414682225320335218, -0.655962405434800777, -0.630675807891475371),
  ( 0.515455959944041808,  0.381716898287133011, -0.767200992517747538),
  (-0.355781402532944713,  0.843580002466178147, -0.402234226602925571),
  (-0.995009439436241649,  0.091347795276427931, -0.040147175877166645),
  (-0.518836730327364437, -0.835420380378235850, -0.181331837557262454),
  (-0.420152426708710003, -0.078145249402782959, -0.904082550615019298)
)

// cf. https://en.wikipedia.org/wiki/Icosahedron
val iso = Vector(
  (0.0, +1.0, +phi),
  (0.0, -1.0, +phi),
  (0.0, +1.0, -phi),
  (0.0, -1.0, -phi),
  (+1.0, +phi, 0.0),
  (-1.0, +phi, 0.0),
  (+1.0, -phi, 0.0),
  (-1.0, -phi, 0.0),
  (+phi, 0.0, +1.0),
  (-phi, 0.0, +1.0),
  (+phi, 0.0, -1.0),
  (-phi, 0.0, -1.0)
)

val isoN = iso.map(i => mul(i, 1.0/n))

// iso.map(mag).foreach(println)

assert(iso == iso.distinct)


def calcError(in: Vector[Pt], theta: Double): Double = {
  val r = in.map(rotateX(_, theta))
  val s = r.map(v => mul(v, 1.0/n))
  val e = s.map { i => gray.map(g => mag(sub(g, i))).min }
  e.sum
}

// calcError(iso, 0.5.atan)

def calcError2(in: Vector[Pt], theta: Double): Double = {
  val r0 = in.map(rotateX(_, 0.5.atan))
  val r  = r0.map(rotateY(_, theta))
  val s  = r.map(v => mul(v, 1.0/n))
  val e  = s.map { i => gray.map(g => mag(sub(g, i))).min }
  e.sum
}

// calcError2(iso, 0.446)
// calcError2(iso, 0.5.atan)

def calcError3(in: Vector[Pt], theta: Double): Double = {
  val r0 = in.map(rotateX(_, 0.5.atan))
  val r1 = r0.map(rotateY(_, 0.5.atan))
  val r  = r1.map(rotateZ(_, theta))
  val s  = r.map(v => mul(v, 1.0/n))
  val e  = s.map { i => gray.map(g => mag(sub(g, i))).min }
  e.sum
}

// calcError3(iso, 0.15)

implicit class Degrees(private val in: Double) {
  def deg2rad: Double = in * math.Pi/180
  def rad2deg: Double = in * 180/math.Pi
}

import concurrent._
import ExecutionContext.Implicits.global

gray.map(mag).sum  // 12.0
iso .map(i => mag(mul(i, 1.0/n))).sum // 12.0

import de.sciss.kollflitz.Ops._

def exhaust(x: Unit): Future[(Pt, Double)] = Future {
  var best    = (0.0, 0.0, 0.0)
  var bestErr = Double.PositiveInfinity
  
  def printBest() = println(s"best = $best, err = $bestErr")
  
  val ext  = 45.deg2rad
  val fine = 0.01.deg2rad
  var prog = 0
  
  for (x <- -ext to ext by fine) {
    for (y <- -ext to ext by fine) {
      val z = 0.0 // for (z <- 0.0 to 0.0 /* -ext to ext by fine */) {
        val r0 = isoN.map(rotateX(_, x))
        val r1 = r0  .map(rotateY(_, y))
        val s  = r1  .map(rotateZ(_, z))
        val e  = s.map { i => gray.map(g => mag(sub(g, i))).min }
        val err= e.sum
        // val es = e.sortedT
        // val err = es.median
        if (err < bestErr) {
          bestErr = err
          best    = (x, y, z)
          printBest()
        }
      // }
    }
    val p = x.linlin(-ext, ext, 0, 100).toInt
    if (p > prog) {
      prog = p
      println(s"$p%")
    }
  }
  // printBest()
  println("Done.")
  (best, bestErr)
}

val fut = exhaust(())

val ((ax, ay, az), ae) = fut.value.get.get
ax.rad2deg
ay.rad2deg
az.rad2deg


ax/math.Pi
ay/math.Pi


val rot  = isoN.map(i => rotateY(rotateX(i, ax), ay))
// val rotE = rot.map { i => gray.map(g => mag(sub(g, i))).min }
// rotE.foreach(println)
val grayE = gray.map { i => rot.map(g => mag(sub(g, i))).min }
grayE.foreach(println)

// there are strong discrepancies for vertices 3, 6, 9, 11 (1-based)

def distances(in: Vector[Pt]) = in.map { a =>
  in.collect {
     case b if a != b => mag(sub(a, b))
  } .min
}

distances(isoN).foreach(println)
distances(iso).foreach(println)

/////////

val rotX = -0.16196655458511255
val rotY = -0.5892231554732978

x0*phi
y0*phi

// best = (-0.16196655458511255,-0.5892231554732978,0.0), err = 0.0731504945050

////////////////////////////

val faces = Vector(
  ( 0,  1,  2), ( 0, 2,  3), ( 0,  3,  4), ( 0,  4,  5), ( 0,  5,  1),
  ( 1,  2,  7), ( 7, 2,  8), ( 8,  2,  3), ( 9,  8,  3), ( 4,  9,  3),
  ( 4, 10,  9), ( 4, 5, 10), (10,  5,  6), ( 6,  5,  1), ( 7,  6,  1),
  (11,  8,  7), (11, 8,  9), (11, 10,  9), (11, 10,  6), (11,  7,  6)
)

assert(faces.flatMap { case (a, b, c) => Seq(a, b, c) } .counted.values.forall(_ == 5))

val mid = faces.map { case (a, b, c) =>
  val (x1, y1, z1) = gray(a)
  val (x2, y2, z2) = gray(b)
  val (x3, y3, z3) = gray(c)
  ((x1 + x2 + x3)/3, (y1 + y2 + y3)/3, (z1 + z2 + z3)/3)
}

val isoR = isoN.map(i => rotateY(rotateX(i, rotX), rotY))
val idxMap = gray.map { i =>
  val m = isoR.map(g => mag(sub(g, i)))
  m.indexOf(m.min)
}

val facesI = faces.map { case (a, b, c) =>
  val Seq(a1, b1, c1) = Seq(idxMap(a), idxMap(b), idxMap(c)).sorted
  (a1, b1, c1)
} .sorted

val center = facesI.map { case (a, b, c) =>
  val (x1, y1, z1) = isoR(a)
  val (x2, y2, z2) = isoR(b)
  val (x3, y3, z3) = isoR(c)
  val cx = (x1 + x2 + x3)/3
  val cy = (y1 + y2 + y3)/3
  val cz = (z1 + z2 + z3)/3
  val m  = mag((cx, cy, cz))
  (cx / m, cy / m, cz / m)
}

center.foreach(c => println(mag(c))) // 1.0

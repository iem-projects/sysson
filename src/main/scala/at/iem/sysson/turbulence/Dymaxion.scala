package at.iem.sysson.turbulence

import de.sciss.numbers
import numbers.Implicits._
import math.{sqrt, sin, cos, asin, acos, atan, atan2, Pi}

// death penality for C programmers who mutate arguments, re-use variables and use 1-based indices

/** Utility functions for Dymaxion(TM) projections. */
object Dymaxion {
  private final val phi = (1 + sqrt(5)) / 2

  type Pt2 = (Double, Double)
  type Pt3 = (Double, Double, Double)

  // cf. https://en.wikipedia.org/wiki/Icosahedron
  // cartesian coordinates of the 12 vertices
  private val iso0 = Vector[Pt3](
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

  // these rotations were applied by Gray;
  // why exactly this amount, I don't know,
  // apparently that's what Fuller used?
  private val rotX =  -9.28 // degrees
  private val rotY = -33.76 // degrees

  private val isoR0 = iso0.map(i => rotateY(rotateX(i, rotX.toRadians), rotY.toRadians))

  private val normFactor  = 1.0 / mag((0, 1, phi))
  private val isoR_FOO    = isoR0.map(i => mul(i, normFactor))

  private val isoR = Vector(
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

  private val faces_FOO = Vector(
    ( 0, 1, 8), ( 0, 1, 9), ( 0, 4, 5), ( 0, 4, 8), ( 0, 5, 9),
    ( 1, 6, 7), ( 1, 6, 8), ( 1, 7, 9), ( 2, 3,10), ( 2, 3,11),
    ( 2, 4, 5), ( 2, 4,10), ( 2, 5,11), ( 3, 6, 7), ( 3, 6,10),
    ( 3, 7,11), ( 4, 8,10), ( 5, 9,11), ( 6, 8,10), ( 7, 9,11)
  )

  private val faces = Vector(
    ( 1, 2, 3), ( 1, 3, 4), ( 1,  4, 5), ( 1,  5, 6), ( 1, 2, 6),
    ( 2, 3, 8), ( 8, 3, 9), ( 9,  3, 4), (10,  9, 4), ( 5,10, 4),
    ( 5,11,10), ( 5, 6,11), (11,  6, 7), ( 7,  6, 2), ( 8, 7, 2),
    (12, 9, 8), (12, 9,10), (12, 11,10), (12, 11, 7), (12, 8, 7)
  ) .map { case (a,b,c) => (a-1, b-1, c-1) }

  private def mag(in: Pt3): Double = {
    val (x, y, z) = in
    (x.squared + y.squared + z.squared).sqrt
  }

  private def mul(in: Pt3, f: Double): Pt3 = {
    val (x, y, z) = in
    (x * f, y * f, z *f)
  }

  private def sub(a: Pt3, b: Pt3): Pt3 = {
    val (x1, y1, z1) = a
    val (x2, y2, z2) = b
    (x2 - x1, y2 - y1, z2 - z1)
  }

  // cartesian coordinates of the centers of the 20 faces
  val center: Vector[Pt3] = faces.map { case (a, b, c) =>
    val (x1, y1, z1) = isoR(a)
    val (x2, y2, z2) = isoR(b)
    val (x3, y3, z3) = isoR(c)
    val cx = (x1 + x2 + x3)/3
    val cy = (y1 + y2 + y3)/3
    val cz = (z1 + z2 + z3)/3
    val m  = mag((cx, cy, cz))
    (cx / m, cy / m, cz / m)
  }

  private def lonLatToCartesian(lon: Double, lat: Double): Pt3 = {
    val (theta, phi)  = lonLatToPolar(lon, lat)
    polarToCartesian(theta, phi)
  }

  def findFaceIndex(lon: Double, lat: Double): Int /* (Double, Double) */ = {
    val pt = lonLatToCartesian(lon, lat)
    findFaceIndex(pt)
  }

  private val gArc  = 2.0 * asin(sqrt(5 - sqrt(5)) / sqrt(10))
  private val gt    = gArc / 2
  private val gDve  = sqrt(3 + sqrt(5)) / sqrt(5 + sqrt(5))
  private val gEl   = sqrt(8) / sqrt(5 + sqrt(5))

  def mapLonLat(lon: Double, lat: Double): Pt2 = {
    val pt = lonLatToCartesian(lon, lat)
    mapCartesian(pt)
  }

  private val face1Gray = Vector(1, 1, 1, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6, 2, 2, 8, 9, 10, 11, 8).map(_ - 1)

  def mapCartesian(h0: Pt3): Pt2 = {
    val tri = findFaceIndex(h0)

    // val vIdx    = faces(faceIdx)._1
    val v1 = face1Gray(tri)

    val h1      = isoR(v1)      // cartesian coordinates of one of the face's vertices
    val c       = center(tri) // cartesian coordinates of        the face's center

    val (hlng, hlat)    = c_to_s(c)
    //    val ptNorm            = rotateY(rotateZ(pt, -cPhi), -cTheta)
    //    val vNorm             = rotateY(rotateZ(v , -cPhi), -cTheta)
    val h0_b = r2(3, hlng, h0)
    val h1_b = r2(3, hlng, h1)

    val h0_c = r2(2, hlat, h0_b)
    val h1_c = r2(2, hlat, h1_b)

    val (hlng_b, hlat_b) = c_to_s(h1_c)
    val hlng_c = hlng_b - 90.0.toRadians

    val h0_d = r2(3,hlng_c,h0_c);

    //    val ptNorm            = rotateY_Gray(rotateZ_Gray(/* c */ h0, hlng), hlat)
    //    val vNorm             = rotateY_Gray(rotateZ_Gray(h1 , hlng), hlat)

//    val (_, vPhiN)        = cartesianToPolar(vNorm)
//    // val (h0x, h0y, _)     = rotateZ(ptNorm, Pi - vPhiN)
//    val (h0x, h0y, _)     = rotateZ_Gray(ptNorm, vPhiN - Pi)

    val (h0x, h0y, h0z) = h0_d

    val gz = sqrt(1 - h0x * h0x - h0y * h0y)
    val gs = sqrt(5 + 2 * sqrt(5)) / (gz * sqrt(15))

    val gxp = h0x * gs
    val gyp = h0y * gs

    val ga1p =                    2*gyp / sqrt(3)  + (gEl / 3)
    val ga2p =             gxp - (  gyp / sqrt(3)) + (gEl / 3)
    val ga3p = (gEl / 3) - gxp - (  gyp / sqrt(3))

    val ga1 = gt + atan((ga1p - 0.5 * gEl) / gDve)
    val ga2 = gt + atan((ga2p - 0.5 * gEl) / gDve)
    val ga3 = gt + atan((ga3p - 0.5 * gEl) / gDve)

    val gx = 0.5 * (ga2 - ga3)
    val gy = (1.0 / (2.0 * sqrt(3))) * (2 * ga1 - ga2 - ga3)

    // Re-scale so plane triangle edge length is 1.

    val x = gx / gArc
    val y = gy / gArc

    val ang = triRota_GRAY(tri).toRadians
    val xr  = x * cos(ang) - y * sin(ang)
    val yr  = x * sin(ang) + y * cos(ang)

    val (addX, addY) = triPos_GRAY(tri)
    (xr + addX, yr + addY)

    //    val (vx, vyi) = triPos(tri)
    //    val vy  = vyi * 2 + (vx % 2)
    //
    //    val korr = 1.0 / 0.8660254037844386 // XXX TODO
    //    (vx + xr * 2, vy - yr * 3 * korr)
  }

  private val triRota_FOO = Vector(
    //  0  1  2  3  4   5   6   7   8  9 10 11 12  13   14   15 16 17 18  19
    20, 0, 0, 0, 0, 30, 90, 330, 0, 0, 0, 0, 0, 150, 90, 210, 0, 0, 0, 270
  )

  private val triRota_GRAY = Vector(
    //  0    1  2   3     4   5    6  7  8             9  10   11  12  13  14  15          16  17   18  19
    240, 300, 0, 60, 180, 300, 300, 0, 0 /* 300 */, 60, 60, 120, 60, 0,  0,  0 /* 60 */, 0, 120, 120, 300
  )

  private val triRota = Vector(
    //  0    1  2   3     4   5    6  7  8             9  10   11  12   13  14  15          16  17   18  19
      240, 300, 0, 60, 180, 300, 300, 0, 0 /* 300 */, 60, 60, 120, 150, 0,  0,  0 /* 60 */, 0, 120, 120, 300
  )

  // maps between face indices and dymaxion coordinates, as
  // corresponding to the labels in the dymaxion view (vx, vyi)
  private val triPos_FOO = Vector(
    // 0       1       2       3      4       5       6       7       8       9
    (7, 2), (7, 3), (5, 2), (6, 2), (5, 3), (9, 2), (8, 2), (9, 3), (1, 3), (2, 4),
    //10      11      12      13      14       15       16      17      18      19
    (4, 2), (5, 0), (3, 3), (11, 2), (12, 2), (11, 3), (6, 1), (4, 4), (7, 0), (10, 4)
  )

  private val triPos_GRAY = Vector(
    (2.0, 7.0 / (2.0 * sqrt(3.0))),
    (2.0, 5.0 / (2.0 * sqrt(3.0))),
    (2.5, 2.0 / sqrt(3.0)),
    (3.0, 5.0 / (2.0 * sqrt(3.0))),
    (2.5, 4.0 * sqrt(3.0) / 3.0),
    (1.5, 4.0 * sqrt(3.0) / 3.0),
    (1.0, 5.0 / (2.0 * sqrt(3.0))),
    (1.5, 2.0 / sqrt(3.0)),
    (2.0, 1.0 / (2.0 * sqrt(3.0))),  // X
    (2.5, 1.0 / sqrt(3.0)),
    (3.5, 1.0 / sqrt(3.0)),
    (3.5, 2.0 / sqrt(3.0)),
    (4.0, 5.0 / (2.0 * sqrt(3.0))),
    (4.0, 7.0 / (2.0 * sqrt(3.0))),
    (5.0, 7.0 / (2.0 * sqrt(3.0))),
    (5.5, 2.0 / sqrt(3.0)),  // X
    (1.0, 1.0 / (2.0 * sqrt(3.0))),
    (4.0, 1.0 / (2.0 * sqrt(3.0))),
    (4.5, 2.0 / sqrt(3.0)),
    (5.0, 5.0 / (2.0 * sqrt(3.0)))
  )

  private val triPos = Vector(
    // 0       1       2       3      4       5       6       7       8       9
    (7, 2), (7, 3), (5, 2), (6, 2), (5, 3), (9, 2), (8, 2), (9, 3), (1, 3), (2, 4),
    //10      11      12      13      14       15       16      17      18      19
    (4, 2), (5, 0), (11, 2), (3, 3), (12, 2), (11, 3), (6, 1), (4, 4), (7, 0), (10, 4)
  )

  // val triRota = Vector(240, 300, 0, 60, 180, 300, 300, 0, 0 /* 300 */, 60, 60, 120, 60, 0, 0, 60 /* 0 */, 0, 120, 120, 300)

  private def findFaceIndex(pt: Pt3): Int =
    center.iterator.zipWithIndex.minBy {
      case (cp, ci) => mag(sub(cp, pt))
    } ._2

  private def rotateX(in: Pt3, theta: Double): Pt3 = {
    // 1 0    0
    // 0 cos -sin
    // 0 sin  cos

    val (x, y, z) = in
    val cs        = cos(theta)
    val sn        = sin(theta)
    (x, y * cs - z * sn, y * sn + z * cs)
  }

  private def rotateY(in: Pt3, theta: Double): Pt3 = {
    // cos  0 sin
    // 0    1 0
    // -sin 0 cos

    val (x, y, z) = in
    val cs       = cos(theta)
    val sn       = sin(theta)
    (x * cs + z * sn, y, -x * sn + z * cs)
  }

  private def rotateY_Gray(in: Pt3, theta: Double): Pt3 = {
    val (x, y, z) = in
    val cs       = cos(theta)
    val sn       = sin(theta)
    (x * cs - z * sn, y, x * sn + z * cs)
  }

  private def rotateZ(in: Pt3, theta: Double): Pt3 = {
    // cos -sin 0
    // sin  cos 0
    // 0    0   1

    val (x, y, z) = in
    val cs        = cos(theta)
    val sn        = sin(theta)
    (x * cs - y * sn, x * sn + y * cs, z)
  }

  private def rotateZ_Gray(in: Pt3, theta: Double): Pt3 = {
    val (x, y, z) = in
    val cs        = cos(theta)
    val sn        = sin(theta)
    (x * cs + y * sn, -x * sn + y * cs, z)
  }

  private def r2(axis: Int, alpha: Double, in: Pt3): Pt3 = {
    var (x, y, z) = in
    val a = x
    val b = y
    val c = z
    if (axis == 1)
    {
      y = b * cos(alpha) + c * sin(alpha);
      z = c * cos(alpha) - b * sin(alpha);
    }

    if (axis == 2)
    {
      x = a * cos(alpha) - c * sin(alpha);
      z = a * sin(alpha) + c * cos(alpha);
    }

    if (axis == 3)
    {
      x = a * cos(alpha) + b * sin(alpha);
      y = b * cos(alpha) - a * sin(alpha);
    }
    (x, y, z)
  }

  private def lonLatToPolar(lon: Double, lat: Double): (Double, Double) = {
    val theta = (90 - lat).toRadians
    val phi   = ((lon + 360) % 360).toRadians
    (theta, phi)
  }

  private def polarToCartesian(theta: Double, phi: Double): Pt3 = {
    val x = sin(theta) * cos(phi)
    val y = sin(theta) * sin(phi)
    val z = cos(theta)
    (x, y, z)
  }

  private def cartesianToPolar_FOO(pt: Pt3): (Double, Double) = {
    val (x, y, z) = pt
    (acos(z), atan2(y, x))
  }

  private def c_to_s(pt: Pt3): (Double, Double) = {
    var a = 0.0
    val (x, y, z) = pt

    def radians(d: Double) = d.toRadians

    if (x>0.0 && y>0.0){a = radians(0.0);}
    if (x<0.0 && y>0.0){a = radians(180.0);}
    if (x<0.0 && y<0.0){a = radians(180.0);}
    if (x>0.0 && y<0.0){a = radians(360.0);}
    val lat = acos(z)
    var lng = 0.0
    if (x==0.0 && y>0.0){ lng = radians(90.0);}
    if (x==0.0 && y<0.0){ lng = radians(270.0);}
    if (x>0.0 && y==0.0){ lng = radians(0.0);}
    if (x<0.0 && y==0.0){ lng = radians(180.0);}
    if (x!=0.0 && y!=0.0){ lng = atan(y/x) + a;}

    (lng, lat)
  }
}
package at.iem.sysson.turbulence

import de.sciss.numbers
import numbers.Implicits._
import math.{sqrt, sin, cos, asin, acos, atan, atan2, Pi}

/** Utility functions for Dymaxion(TM) projections. */
object Dymaxion {
  private final val phi = (1 + sqrt(5)) / 2

  type Pt2 = (Double, Double)
  type Pt3 = (Double, Double, Double)

  // cf. https://en.wikipedia.org/wiki/Icosahedron
  // cartesian coordinates of the 12 vertices
  private val iso = Vector[Pt3](
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

  private val isoR = iso.map(i => rotateY(rotateX(i, rotX.toRadians), rotY.toRadians))

  private val faces = Vector(
    ( 0, 1, 8), ( 0, 1, 9), ( 0, 4, 5), ( 0, 4, 8), ( 0, 5, 9),
    ( 1, 6, 7), ( 1, 6, 8), ( 1, 7, 9), ( 2, 3,10), ( 2, 3,11),
    ( 2, 4, 5), ( 2, 4,10), ( 2, 5,11), ( 3, 6, 7), ( 3, 6,10),
    ( 3, 7,11), ( 4, 8,10), ( 5, 9,11), ( 6, 8,10), ( 7, 9,11)
  )

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

  def mapCartesian(pt: Pt3): Pt2 = {
    val faceIdx = findFaceIndex(pt)

    val vIdx    = faces(faceIdx)._1
    val v       = isoR(vIdx)      // cartesian coordinates of one of the face's vertices
    val c       = center(faceIdx) // cartesian coordinates of        the face's center

    val (cTheta, cPhi)    = cartesianToPolar(c)
    val ptNorm            = rotateY(rotateZ(pt, -cPhi), -cTheta)
    val vNorm             = rotateY(rotateZ(v , -cPhi), -cTheta)

    val (_, vPhiN)        = cartesianToPolar(vNorm)
    val (h0x, h0y, _)     = rotateZ(ptNorm, Pi - vPhiN)

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

    val x = gx / gArc    // *  2 to have the correct orientation wrt dymaxion view
    val y = gy / gArc    // * -2 to have the correct orientation wrt dymaxion view

    val (vx, vyi) = triPos(faceIdx)
    val vy  = vyi * 2 + (vx % 2)

    val ang = triRota(faceIdx).toRadians
    val xr  = x * cos(ang) - y * sin(ang)
    val yr  = x * sin(ang) + y * cos(ang)

    val korr = 1.0 / 0.8660254037844386 // XXX TODO

    (vx + xr * 2 * korr, vy - yr * 3 / korr)
  }

  private val triRota = Vector(
    //  0  1  2  3  4   5   6   7   8  9 10 11 12  13   14   15 16 17 18  19
       20, 0, 0, 0, 0, 30, 90, 330, 0, 0, 0, 0, 0, 150, 90, 210, 0, 0, 0, 270
  )

  // maps between face indices and dymaxion coordinates, as
  // corresponding to the labels in the dymaxion view (vx, vyi)
  private val triPos = Vector(
    // 0       1       2       3      4       5       6       7       8       9
    (7, 2), (7, 3), (5, 2), (6, 2), (5, 3), (9, 2), (8, 2), (9, 3), (1, 3), (2, 4),
    //10      11      12      13      14       15       16      17      18      19
    (4, 2), (5, 0), (3, 3), (11, 2), (12, 2), (11, 3), (6, 1), (4, 4), (7, 0), (10, 4)
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

  private def rotateZ(in: Pt3, theta: Double): Pt3 = {
    // cos -sin 0
    // sin  cos 0
    // 0    0   1

    val (x, y, z) = in
    val cs        = cos(theta)
    val sn        = sin(theta)
    (x * cs - y * sn, x * sn + y * cs, z)
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

  private def cartesianToPolar(pt: Pt3): (Double, Double) = {
    val (x, y, z) = pt
    (acos(z), atan2(y, x))
  }
}
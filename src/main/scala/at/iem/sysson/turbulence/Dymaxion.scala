package at.iem.sysson.turbulence

import math.{sqrt, sin, cos, asin, acos, atan, atan2}

/** Utility functions for Dymaxion(TM) projections. */
object Dymaxion {
  private final val phi = (1 + sqrt(5)) / 2

  final case class Pt3(x: Double, y: Double, z: Double)
  final case class Pt2(x: Double, y: Double)

  final case class Polar(theta: Double, phi: Double)

  // cf. https://en.wikipedia.org/wiki/Icosahedron
  // cartesian coordinates of the 12 vertices
  private val iso0 = Array[Pt3](
    Pt3(0.0, +1.0, +phi),
    Pt3(0.0, -1.0, +phi),
    Pt3(0.0, +1.0, -phi),
    Pt3(0.0, -1.0, -phi),
    Pt3(+1.0, +phi, 0.0),
    Pt3(-1.0, +phi, 0.0),
    Pt3(+1.0, -phi, 0.0),
    Pt3(-1.0, -phi, 0.0),
    Pt3(+phi, 0.0, +1.0),
    Pt3(-phi, 0.0, +1.0),
    Pt3(+phi, 0.0, -1.0),
    Pt3(-phi, 0.0, -1.0)
  )

  // these rotations were applied by Gray;
  // why exactly this amount, I don't know,
  // apparently that's what Fuller used?
  private val rotX =  -9.28 // degrees
  private val rotY = -33.76 // degrees

  private val isoR0: Array[Pt3] = iso0.map(i => rotateY(rotateX(i, rotX.toRadians), rotY.toRadians))

  private val normFactor  = 1.0 / mag(Pt3(0, 1, phi))
  private val isoR        = isoR0.map(i => mul(i, normFactor))

  private val faces = Array[(Int, Int, Int)](
    ( 0, 1, 8), ( 0, 1, 9), ( 0, 4, 5), ( 0, 4, 8), ( 0, 5, 9),
    ( 1, 6, 7), ( 1, 6, 8), ( 1, 7, 9), ( 3,10, 2), ( 3,11, 2),
    ( 4, 5, 2), ( 4,10, 2), ( 5,11, 2), ( 3, 6, 7), ( 3, 6,10),
    ( 7,11, 3), ( 8,10, 4), ( 9,11, 5), (10, 6, 8), ( 7, 9,11)
  )

  private def mag(in: Pt3): Double = sqrt(in.x * in.x + in.y * in.y + in.z * in.z)

  private def mul(in: Pt3, f: Double): Pt3 = Pt3(in.x * f, in.y * f, in.z *f)

  private def sub(a: Pt3, b: Pt3): Pt3 = Pt3(b.x - a.x, b.y - a.y, b.z - a.z)

  // cartesian coordinates of the centers of the 20 faces
  private val center: Array[Pt3] = faces.map { case (a, b, c) =>
    val pa = isoR(a)
    val pb = isoR(b)
    val pc = isoR(c)
    val cx = (pa.x + pb.x + pc.x)/3
    val cy = (pa.y + pb.y + pc.y)/3
    val cz = (pa.z + pb.z + pc.z)/3
    val m  = mag(Pt3(cx, cy, cz))
    Pt3(cx / m, cy / m, cz / m)
  }

  private def lonLatToCartesian(lon: Double, lat: Double): Pt3 = {
    val p = lonLatToPolar(lon, lat)
    polarToCartesian(p)
  }

  def findFaceIndex(lon: Double, lat: Double): Int = {
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

  def mapCartesian(h0: Pt3): Pt2 = {
    val faceIdx = findFaceIndex(h0)

    val vIdx    = faces(faceIdx)._1
    val h1      = isoR  (vIdx)      // cartesian coordinates of one of the face's vertices
    val c       = center(faceIdx)   // cartesian coordinates of        the face's center

    val Polar(hLat, hLng) = cartesianToPolar(c)
    //    val ptNorm            = rotateY(rotateZ(pt, -cPhi), -cTheta)
    //    val vNorm             = rotateY(rotateZ(v , -cPhi), -cTheta)
    val h0_b = r2(3, hLng, h0)
    val h1_b = r2(3, hLng, h1)

    val h0_c = r2(2, hLat, h0_b)
    val h1_c = r2(2, hLat, h1_b)

    val Polar(hlat_b, hlng_b) = cartesianToPolar(h1_c)
    val hlng_c = hlng_b - 90.0.toRadians

    val h0_d = r2(3,hlng_c,h0_c)

    //    val ptNorm            = rotateY_Gray(rotateZ_Gray(/* c */ h0, hlng), hlat)
    //    val vNorm             = rotateY_Gray(rotateZ_Gray(h1 , hlng), hlat)

//    val (_, vPhiN)        = cartesianToPolar(vNorm)
//    // val (h0x, h0y, _)     = rotateZ(ptNorm, Pi - vPhiN)
//    val (h0x, h0y, _)     = rotateZ_Gray(ptNorm, vPhiN - Pi)

    val Pt3(h0x, h0y, _) = h0_d

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

    // Normalize to triangle edge length 1
    val x = gx / gArc
    val y = gy / gArc

    val (vx, vyi, angDeg) = triPos(faceIdx)

    val ang = angDeg.toRadians
    val xr  = x * cos(ang) - y * sin(ang)
    val yr  = x * sin(ang) + y * cos(ang)
    val vy  = vyi * 2 + (vx % 2)

    Pt2(vx + xr * xScale, vy - yr * yScale)
  }

  private final val xScale = 2
  private final val yScale = 2 * sqrt(3)

  // maps between face indices and dymaxion coordinates, as
  // corresponding to the labels in the dymaxion view (vx, vyi)
  private val triPos = Vector(
    //  0            1           2            3            4
    (7, 2, 120), (7, 3, 60), (5, 2, 240), (6, 2, 180), (5, 3, 300),
    //  5            6            7           8            9
    (9, 2, 120), (8, 2, 180), (9, 3, 60), (1, 3, 180), (2, 4, 120),
    // 10            11          12            13            14
    (4, 2, 300), (5, 0, 180), (3, 3, 300), (11, 2, 240), (12, 2, 180),
    //  15          16           17           18           19
    (11, 3, 60), (6, 1, 240), (4, 4, 240), (7, 0, 60), (10, 4, 0)
  )

  private def findFaceIndex(pt: Pt3): Int =
    center.iterator.zipWithIndex.minBy {
      case (cp, ci) => mag(sub(cp, pt))
    } ._2

  private def rotateX(in: Pt3, theta: Double): Pt3 = {
    // 1 0    0
    // 0 cos -sin
    // 0 sin  cos

    val cs        = cos(theta)
    val sn        = sin(theta)
    Pt3(in.x, in.y * cs - in.z * sn, in.y * sn + in.z * cs)
  }

  private def rotateY(in: Pt3, theta: Double): Pt3 = {
    // cos  0 sin
    // 0    1 0
    // -sin 0 cos

    val cs       = cos(theta)
    val sn       = sin(theta)
    Pt3(in.x * cs + in.z * sn, in.y, -in.x * sn + in.z * cs)
  }

  private def rotateY_Gray(in: Pt3, theta: Double): Pt3 = {
    val cs       = cos(theta)
    val sn       = sin(theta)
    Pt3(in.x * cs - in.z * sn, in.y, in.x * sn + in.z * cs)
  }

  private def rotateZ(in: Pt3, theta: Double): Pt3 = {
    // cos -sin 0
    // sin  cos 0
    // 0    0   1

    val cs        = cos(theta)
    val sn        = sin(theta)
    Pt3(in.x * cs - in.y * sn, in.x * sn + in.y * cs, in.z)
  }

  private def rotateZ_Gray(in: Pt3, theta: Double): Pt3 = {
    val cs        = cos(theta)
    val sn        = sin(theta)
    Pt3(in.x * cs + in.y * sn, -in.x * sn + in.y * cs, in.z)
  }

  private def r2(axis: Int, alpha: Double, in: Pt3): Pt3 = {
    var x = in.x
    var y = in.y
    var z = in.z
    val a = in.x
    val b = in.y
    val c = in.z
    if (axis == 1)
    {
      y = b * cos(alpha) + c * sin(alpha)
      z = c * cos(alpha) - b * sin(alpha)
    }

    if (axis == 2)
    {
      x = a * cos(alpha) - c * sin(alpha)
      z = a * sin(alpha) + c * cos(alpha)
    }

    if (axis == 3)
    {
      x = a * cos(alpha) + b * sin(alpha)
      y = b * cos(alpha) - a * sin(alpha)
    }
    Pt3(x, y, z)
  }

  private def lonLatToPolar(lon: Double, lat: Double): Polar = {
    val theta = (90 - lat).toRadians
    val phi   = ((lon + 360) % 360).toRadians
    Polar(theta, phi)
  }

  private def polarToCartesian(in: Polar): Pt3 = {
    val x = sin(in.theta) * cos(in.phi)
    val y = sin(in.theta) * sin(in.phi)
    val z = cos(in.theta)
    Pt3(x, y, z)
  }

  private def cartesianToPolar(in: Pt3): Polar = Polar(acos(in.z), atan2(in.y, in.x))
}
/*
 *  Dymaxion.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson.turbulence

import math.{sqrt, sin, cos, asin, acos, atan, atan2}

/** Utility functions for Dymaxion(TM) projections. */
object Dymaxion {
  private final val phi = (1 + sqrt(5)) / 2

  /** Horizontal scale factor */
  final val hScale = 92
  /** Vertical scale factor */
  final val vScale = 53.33333f

  final case class Pt3(x: Double, y: Double, z: Double)

  final case class Pt2(x: Double, y: Double) {
    def distanceTo(that: Pt2): Double = {
      val dx = that.x - this.x
      val dy = that.y - this.y
      sqrt(dx * dx + dy * dy)
    }
  }

  final case class DymPt(x: Double, y: Double) {
    def equalize: Pt2 = Pt2(x * hScale, y * vScale)
  }

  final case class Polar(theta: Double, phi: Double)

  private final case class Pos(x: Int, yi: Int, rot: Int)

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
  private val rotX =  9.28 // degrees
  private val rotY = 33.76 // degrees

  private val isoR0: Array[Pt3] = iso0.map(i => rotateY(rotateX(i, rotX.toRadians), rotY.toRadians))

  private val normFactor  = 1.0 / mag(Pt3(0, 1, phi))
  private val isoR        = isoR0.map(i => mul(i, normFactor))

  private final case class Fc(_1: Int, _2: Int, _3: Int)

  private val faces = Array[Fc](
    Fc( 0, 1, 8), Fc( 0, 1, 9), Fc( 0, 4, 5), Fc( 0, 4, 8), Fc( 0, 5, 9),
    Fc( 1, 6, 7), Fc( 1, 6, 8), Fc( 1, 7, 9), Fc( 3,10, 2), Fc( 3,11, 2),
    Fc( 4, 5, 2), Fc( 4,10, 2), Fc( 5,11, 2), Fc( 3, 6, 7), Fc( 3, 6,10),
    Fc( 7,11, 3), Fc( 8,10, 4), Fc( 9,11, 5), Fc(10, 6, 8), Fc( 7, 9,11)
  )

  private def mag(in: Pt3): Double = sqrt(in.x * in.x + in.y * in.y + in.z * in.z)

  private def mul(in: Pt3, f: Double): Pt3 = Pt3(in.x * f, in.y * f, in.z *f)

  private def sub(a: Pt3, b: Pt3): Pt3 = Pt3(b.x - a.x, b.y - a.y, b.z - a.z)

  // cartesian coordinates of the centers of the 20 faces
  private val center: Array[Pt3] = faces.map { fc =>
    val pa = isoR(fc._1)
    val pb = isoR(fc._2)
    val pc = isoR(fc._3)
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

  /** Maps a longitude and latitude coordinate to a
    * cartesian coordinate with respect to the Dymaxion projection.
    *
    * @param lon  longitude in degrees
    * @param lat  latitude  in degrees
    */
  def mapLonLat(lon: Double, lat: Double): DymPt = {
    val pt = lonLatToCartesian(lon, lat)
    mapCartesian(pt)
  }

  /** Maps a 3D cartesian coordinate with respect to the globe
    * to a 2D cartesian coordinate with respect to the Dymaxion
    * projection.
    */
  def mapCartesian(pt: Pt3): DymPt = {
    val faceIdx = findFaceIndex(pt)

    val vIdx    = faces(faceIdx)._1
    val v1      = isoR  (vIdx)      // cartesian coordinates of one of the face's vertices
    val c       = center(faceIdx)   // cartesian coordinates of        the face's center

    val Polar(cTheta, cPhi) = cartesianToPolar(c)

    val ptRot = rotateY(rotateZ(pt, cPhi), cTheta)
    val v1Rot = rotateY(rotateZ(v1, cPhi), cTheta)

    val Polar(_, v1Phi)   = cartesianToPolar(v1Rot)
    val Pt3(h0x, h0y, _)  = rotateZ(ptRot, v1Phi - math.Pi/2)

    // Project the spherical triangle to the flat triangle
    val gz   = sqrt(1 - h0x * h0x - h0y * h0y)
    val gs   = sqrt(5 + 2 * sqrt(5)) / (gz * sqrt(15))

    val gxp  = h0x * gs
    val gyp  = h0y * gs

    val ga1p =                    2*gyp / sqrt(3)  + (gEl / 3)
    val ga2p =             gxp - (  gyp / sqrt(3)) + (gEl / 3)
    val ga3p = (gEl / 3) - gxp - (  gyp / sqrt(3))

    val ga1  = gt + atan((ga1p - 0.5 * gEl) / gDve)
    val ga2  = gt + atan((ga2p - 0.5 * gEl) / gDve)
    val ga3  = gt + atan((ga3p - 0.5 * gEl) / gDve)

    val gx   = 0.5 * (ga2 - ga3)
    val gy   = (1.0 / (2.0 * sqrt(3))) * (2 * ga1 - ga2 - ga3)

    // Normalize to triangle edge length 1
    val x    = gx / gArc
    val y    = gy / gArc

    val pos = triPos(faceIdx)

    val ang  = pos.rot.toRadians
    val xr   = x * cos(ang) - y * sin(ang)
    val yr   = x * sin(ang) + y * cos(ang)
    val vy   = pos.yi * 2 + (pos.x % 2)

    DymPt(pos.x + xr * xScale, vy - yr * yScale)
  }

  private final val xScale = 2
  private final val yScale = 2 * sqrt(3)

  // maps between face indices and dymaxion coordinates, as
  // corresponding to the labels in the dymaxion view (vx, vyi)
  private val triPos = Vector(
    //  0               1              2               3               4
    Pos(7, 2, 120), Pos(7, 3, 60), Pos(5, 2, 240), Pos(6, 2, 180), Pos(5, 3, 300),
    //  5                6              7              8               9
    Pos(9, 2, 120), Pos(8, 2, 180), Pos(9, 3, 60), Pos(1, 3, 180), Pos(2, 4, 120),
    //  10              11              12              13               14
    Pos(4, 2, 300), Pos(5, 0, 180), Pos(3, 3, 300), Pos(11, 2, 240), Pos(12, 2, 180),
    //  15              16              17              18             19
    Pos(11, 3, 60), Pos(6, 1, 240), Pos(4, 4, 240), Pos(7, 0, 60), Pos(10, 4, 0)
  )

  private def findFaceIndex(pt: Pt3): Int =
    center.iterator.zipWithIndex.minBy {
      case (cp, ci) => mag(sub(cp, pt))
    } ._2

  private def rotateX(in: Pt3, theta: Double): Pt3 = {
    // 1 0    0
    // 0 cos  sin
    // 0 -sin cos
    val cs        = cos(theta)
    val sn        = sin(theta)
    Pt3(in.x, in.y * cs + in.z * sn, -in.y * sn + in.z * cs)
  }

  private def rotateY(in: Pt3, theta: Double): Pt3 = {
    // cos  0 -sin
    // 0    1 0
    // sin  0 cos
    val cs       = cos(theta)
    val sn       = sin(theta)
    Pt3(in.x * cs - in.z * sn, in.y, in.x * sn + in.z * cs)
  }

  private def rotateZ(in: Pt3, theta: Double): Pt3 = {
    // cos  sin 0
    // -sin cos 0
    // 0    0   1
    val cs        = cos(theta)
    val sn        = sin(theta)
    Pt3(in.x * cs + in.y * sn, -in.x * sn + in.y * cs, in.z)
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
/*
 *  Turbulence.scala
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

package at.iem.sysson
package turbulence

import at.iem.sysson.turbulence.Dymaxion.Pt2
import de.sciss.{numbers, pdflitz}

import java.awt.EventQueue

import scala.swing.Frame

object Turbulence extends Runnable {
  // don't use `App` because body will not be initialized if we don't use it as main entry
  def main(args: Array[String]): Unit = EventQueue.invokeLater(this)

  final case class DynGrid(vx: Int, vyi: Int) {
    def toPoint: Pt2 = {
      val vy = vyi * 2 + (vx % 2)
      Pt2(vx, vy)
    }
  }

  final case class Spk(num: Int) extends AnyVal {
    /** Zero-based channel offset */
    def toIndex = num - 1
  }

  /** Maps from (x, y) index with respect to the
    * equilateral triangle grid to speaker channel
    * (1-based offset)
    */
  final val MatrixToChannelMap = Map[DynGrid, Spk](
    DynGrid( 4, 0) -> Spk( 3),
    DynGrid( 6, 0) -> Spk( 5),
    DynGrid( 8, 0) -> Spk( 4),
    DynGrid( 5, 0) -> Spk( 6),
    DynGrid( 7, 0) -> Spk( 7),
    DynGrid( 6, 1) -> Spk( 8),
    DynGrid( 3, 1) -> Spk(21),
    DynGrid( 5, 1) -> Spk(25),
    DynGrid( 7, 1) -> Spk(26),
    DynGrid( 9, 1) -> Spk(33),
    DynGrid(11, 1) -> Spk(41),
    DynGrid(13, 1) -> Spk(45),
    DynGrid( 4, 2) -> Spk(22),
    DynGrid( 6, 2) -> Spk(27),
    DynGrid( 8, 2) -> Spk(34),
    DynGrid(12, 2) -> Spk(42),
    DynGrid( 5, 2) -> Spk(23),
    DynGrid( 7, 2) -> Spk(29),
    DynGrid( 9, 2) -> Spk(35),
    DynGrid(11, 2) -> Spk(43),
    DynGrid( 0, 3) -> Spk(10),
    DynGrid( 2, 3) -> Spk(13),
    DynGrid( 4, 3) -> Spk(17),
    DynGrid( 6, 3) -> Spk(28),
    DynGrid( 8, 3) -> Spk(30),
    DynGrid(10, 3) -> Spk(37),
    DynGrid(12, 3) -> Spk(46),
    DynGrid( 1, 3) -> Spk(11),
    DynGrid( 3, 3) -> Spk(14),
    DynGrid( 5, 3) -> Spk(24),
    DynGrid( 7, 3) -> Spk(31),
    DynGrid( 9, 3) -> Spk(36),
    DynGrid(11, 3) -> Spk(44),
    DynGrid( 2, 4) -> Spk(15),
    DynGrid( 4, 4) -> Spk(18),
    DynGrid(10, 4) -> Spk(38),
    DynGrid( 1, 4) -> Spk(16),
    DynGrid( 3, 4) -> Spk(19),
    DynGrid( 5, 4) -> Spk(20),
    DynGrid( 7, 4) -> Spk(32),
    DynGrid( 9, 4) -> Spk(39),
    DynGrid(11, 4) -> Spk(40)
  )

  final case class LatLon(lat: Double, lon: Double)
  final case class LatLonIdx(latIdx: Int, lonIdx: Int) {
    def toLatLon = LatLon(latitude(latIdx), longitude(lonIdx))
  }

  /** Maps from speaker channel (1-based offset)
    * to (x, y) index with respect to the
    * equilateral triangle grid
    */
  final val ChannelToMatrixMap: Map[Spk, DynGrid] = MatrixToChannelMap.map(_.swap)

  /** Maps from loudspeaker channels to
    * quantized (lat, lon) pairs. Nearest
    * raster points are used except where
    * there are "jumps" in the Dymaxion,
    * in which case we go "away" a little
    * from the gap, so there are no two
    * loudspeakers with the same geo-coordinates.
    */
  final val ChannelToGeoMap = Map[Spk, LatLon](
    Spk( 3) -> LatLon( -40.0,  +35.0),
    Spk( 6) -> LatLon( -12.5,  +30.0),
    Spk( 5) -> LatLon(  +5.0,    0.0),
    Spk( 7) -> LatLon( +35.0,  -25.0),
    Spk( 4) -> LatLon( +27.5,  -60.0),
    Spk(21) -> LatLon( -45.0,  +45.0),
    Spk(25) -> LatLon( +10.0,  +57.5),
    Spk( 8) -> LatLon( +27.5,  +22.5),
    Spk(26) -> LatLon( +65.0,  +12.5),
    Spk(33) -> LatLon( +27.5,  -67.5),
    Spk(41) -> LatLon( +17.5,  -65.0),
    Spk(45) -> LatLon(  +2.5,  -12.5),
    Spk(22) -> LatLon( -25.0,  +72.5),
    Spk(23) -> LatLon( +10.0,  +97.5),
    Spk(27) -> LatLon( +45.0,  +72.5),
    Spk(29) -> LatLon( +75.0, +142.5),
    Spk(34) -> LatLon( +60.0,  -75.0),
    Spk(35) -> LatLon( +25.0, -107.5),
    Spk(43) -> LatLon( -10.0,  -82.5),
    Spk(42) -> LatLon(  -5.0,  -42.5),
    Spk(10) -> LatLon(  -7.5,   -5.0),
    Spk(11) -> LatLon( -35.0,  -10.0),
    Spk(13) -> LatLon( -52.5,  +27.5),
    Spk(14) -> LatLon( -60.0, +102.5),
    Spk(17) -> LatLon( -25.0, +112.5),
    Spk(24) -> LatLon(  +5.0, +137.5),
    Spk(28) -> LatLon( +40.0, +122.5),
    Spk(31) -> LatLon( +35.0, +170.0),
    Spk(30) -> LatLon( +50.0, -142.5),
    Spk(36) -> LatLon( +12.5, -150.0),
    Spk(37) -> LatLon( -10.0, -122.5),
    Spk(44) -> LatLon( -45.0, -107.5),
    Spk(46) -> LatLon( -37.5,  -60.0),
    Spk(16) -> LatLon( -45.0,  -50.0),
    Spk(15) -> LatLon( -75.0,  -35.0),
    Spk(19) -> LatLon( -67.5, -172.5),
    Spk(18) -> LatLon( -35.0, +152.5),
    Spk(20) -> LatLon(  -7.5, +167.5),
    Spk(32) -> LatLon(  +7.5, +172.5),
    Spk(39) -> LatLon(  -5.0, +180.0),
    Spk(38) -> LatLon( -27.5, -157.5),
    Spk(40) -> LatLon( -62.5, -160.0)
  )

  assert(ChannelToGeoMap.size == 42 && ChannelToGeoMap.valuesIterator.toSet.size == 42)

  private val chans = MatrixToChannelMap.valuesIterator.map(_.num).toVector.sorted
  assert(chans == (3 to 8) ++ (10 to 11) ++ (13 to 46), s"ChannelMap does not have expected values: $chans")

  import numbers.Implicits._

  def latitude (idx: Int): Float = idx.linlin(0,  72,  -90.0f,  +90.0f)
  def longitude(idx: Int): Float = idx.linlin(0, 143, -177.5f, +180.0f)

  final val NumLat =  73
  final val NumLon = 144

  /** Maps latitude index to longitude index to dymaxion coordinate. */
  final val LatLonIndicesToDymMap: Vec[Vec[Pt2]] = Vector.tabulate(NumLat) { latIdx =>
    Vector.tabulate(NumLon) { lonIdx =>
      val lon = longitude(lonIdx)
      val lat = latitude (latIdx)
      Dymaxion.mapLonLat(lat = lat, lon = lon)
    }
  }

  final val LatLonDym: Vec[(LatLonIdx, Pt2)] = LatLonIndicesToDymMap.zipWithIndex.flatMap { case (inLat, latIdx) =>
    inLat.zipWithIndex.map { case (pt, lonIdx) => LatLonIdx(latIdx, lonIdx) -> pt }
  }

  final case class Radians(value: Double) extends AnyVal

  /** Maps latitude index to longitude index to
    * a pair of dymaxion coordinate and northward direction ("compass")
    */
  final val LatLonIndicesToDymCompassMap: Vec[Vec[(Pt2, Radians)]] =
    LatLonIndicesToDymMap.zipWithIndex.map { case (inLat, latIdx) =>
      inLat.zipWithIndex.map { case (pt1, lonIdx) =>
        val latS = latIdx - 1
        val latN = latIdx + 1
        val ptS  = if (latS  <  0) Pt2(-99, -99) else LatLonIndicesToDymMap(latS)(lonIdx)
        val ptN  = if (latN >= 73) Pt2(-99, -99) else LatLonIndicesToDymMap(latN)(lonIdx)

        import DymaxionView.{scaledDist, scalePt}
        val useSouth  = scaledDist(pt1, ptS) < scaledDist(pt1, ptN)
        val pt1Sc     = scalePt(pt1)
        val pt2Sc     = scalePt(if (useSouth) ptS else ptN)
        val ang1      = math.atan2(-(pt2Sc.y - pt1Sc.y), pt2Sc.x - pt1Sc.x)
        val ang       = if (useSouth) (ang1 + math.Pi) % (2 * math.Pi) else ang1

        (pt1, Radians(ang))
      }
    }

  final val ChannelToGeoMap2: Map[Spk, LatLonIdx] = ChannelToMatrixMap.map { case (spk, dyn) =>
    val pt1 = dyn.toPoint
    val nn  = LatLonDym.minBy { case (ll, pt2) =>
      DymaxionView.scaledDist(pt1, pt2)
    }
    spk -> nn._1
  }

  // final val VoronoiMap: Map[Int, Vec[(Int, Int)]]

  def decimate[A](in: Vec[A])(n: Int): Vec[A] = in.iterator.zipWithIndex.collect {
    case (x, i) if i % n == 0 => x
  } .toIndexedSeq

  def run(): Unit = {
    Main.run()

    val dyn     = new DymaxionView
    //    dyn.drawImage    = false
    //    dyn.drawSpeakers = false
    // dyn.crosses = decimate(Preparations.dymGridAng.map(decimate(_)(8)))(4).flatten
    dyn.crosses      = LatLonIndicesToDymCompassMap.flatten
    dyn.mouseControl = false
    val merc    = new MercatorView(dyn)

    val fDyn = new Frame {
      contents = dyn
      resizable = false
    }

    new pdflitz.SaveAction(dyn :: Nil).setupMenu(fDyn)
    fDyn.pack()
    fDyn.centerOnScreen()
    fDyn.open()

    new Frame {
      contents = merc
      resizable = false
      pack()
      centerOnScreen()
      open()
    }
  }
}

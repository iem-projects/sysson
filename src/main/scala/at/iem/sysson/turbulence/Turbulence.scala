/*
 *  Turbulence.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package turbulence

import at.iem.sysson
import at.iem.sysson.turbulence.Dymaxion.{Pt3, Polar, DymPt}
import de.sciss.file._
import de.sciss.lucre.synth.{Group, Txn}
import de.sciss.mellite.Mellite
import de.sciss.swingplus.CloseOperation
import de.sciss.swingplus.Implicits._
import de.sciss.synth.proc.Code
import de.sciss.{numbers, pdflitz, kollflitz}

import ucar.ma2

import scala.swing.event.ButtonClicked
import scala.swing.{ToggleButton, Swing, Frame}
import scala.collection.breakOut
import scala.concurrent.stm.{Ref, atomic}

object Turbulence {
  final case class DymGrid(vx: Int, vyi: Int) {
    def toPoint: DymPt = {
      val vy = vyi * 2 + (vx % 2)
      DymPt(vx, vy)
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
  final val MatrixToChannelMap = Map[DymGrid, Spk](
    DymGrid( 4, 0) -> Spk( 3),
    DymGrid( 6, 0) -> Spk( 5),
    DymGrid( 8, 0) -> Spk( 4),
    DymGrid( 5, 0) -> Spk( 6),
    DymGrid( 7, 0) -> Spk( 7),
    DymGrid( 6, 1) -> Spk( 8),
    DymGrid( 3, 1) -> Spk(21),
    DymGrid( 5, 1) -> Spk(25),
    DymGrid( 7, 1) -> Spk(26),
    DymGrid( 9, 1) -> Spk(33),
    DymGrid(11, 1) -> Spk(41),
    DymGrid(13, 1) -> Spk(45),
    DymGrid( 4, 2) -> Spk(22),
    DymGrid( 6, 2) -> Spk(27),
    DymGrid( 8, 2) -> Spk(34),
    DymGrid(12, 2) -> Spk(42),
    DymGrid( 5, 2) -> Spk(23),
    DymGrid( 7, 2) -> Spk(29),
    DymGrid( 9, 2) -> Spk(35),
    DymGrid(11, 2) -> Spk(43),
    DymGrid( 0, 3) -> Spk(10),
    DymGrid( 2, 3) -> Spk(13),
    DymGrid( 4, 3) -> Spk(17),
    DymGrid( 6, 3) -> Spk(28),
    DymGrid( 8, 3) -> Spk(30),
    DymGrid(10, 3) -> Spk(37),
    DymGrid(12, 3) -> Spk(46),
    DymGrid( 1, 3) -> Spk(11),
    DymGrid( 3, 3) -> Spk(14),
    DymGrid( 5, 3) -> Spk(24),
    DymGrid( 7, 3) -> Spk(31),
    DymGrid( 9, 3) -> Spk(36),
    DymGrid(11, 3) -> Spk(44),
    DymGrid( 2, 4) -> Spk(15),
    DymGrid( 4, 4) -> Spk(18),
    DymGrid(10, 4) -> Spk(38),
    DymGrid( 1, 4) -> Spk(16),
    DymGrid( 3, 4) -> Spk(19),
    DymGrid( 5, 4) -> Spk(20),
    DymGrid( 7, 4) -> Spk(32),
    DymGrid( 9, 4) -> Spk(39),
    DymGrid(11, 4) -> Spk(40)
  )

  final case class LatLon(lat: Double, lon: Double) {
    override def toString = f"[lat: $lat%1.2f, lon: $lon%1.2f]"

    def toPolar: Polar = {
      val theta = (90 - lat).toRadians
      val phi   = ((lon + 360) % 360).toRadians
      Polar(theta, phi)
    }

    def toCartesian: Pt3 = toPolar.toCartesian
  }

  final case class LatLonIdx(latIdx: Int, lonIdx: Int) {
    def toLatLon = LatLon(latitude(latIdx), longitude(lonIdx))
  }

  /** Maps from speaker channel (1-based offset)
    * to (x, y) index with respect to the
    * equilateral triangle grid
    */
  final val ChannelToMatrixMap: Map[Spk, DymGrid] = MatrixToChannelMap.map(_.swap)

  final val Channels: Vec[Spk] = ChannelToMatrixMap.keysIterator.toVector.sortBy(_.num)

  final val ChannelIndices: Vec[Int] = Channels.map(_.toIndex)

  final val NumChannels = ChannelIndices.size

  private val chans = MatrixToChannelMap.valuesIterator.map(_.num).toVector.sorted
  assert(chans == (3 to 8) ++ (10 to 11) ++ (13 to 46), s"ChannelMap does not have expected values: $chans")

  import numbers.Implicits._

  def latitude (idx: Int): Float = idx.linlin(0,  72,  -90.0f,  +90.0f)
  def longitude(idx: Int): Float = idx.linlin(0, 143, -177.5f, +180.0f)

  final val NumLat =  73
  final val NumLon = 144

  final val audioWork = userHome / "IEM" / "SysSon" / "installation" / "audio_work"

  /** Maps latitude index to longitude index to dymaxion coordinate. */
  final val LatLonIndicesToDymMap: Map[LatLonIdx, DymPt] = (0 until NumLat).flatMap { latIdx =>
    (0 until NumLon).map { lonIdx =>
      val lon = longitude(lonIdx)
      val lat = latitude (latIdx)
      val dym = Dymaxion.mapLonLat(LatLon(lat = lat, lon = lon))
      LatLonIdx(latIdx, lonIdx) -> dym
    }
  } (breakOut)

  final val LatLonDym: Vec[(LatLonIdx, DymPt)] = LatLonIndicesToDymMap.toVector

  final case class Radians(value: Double) extends AnyVal {
    /** Wraps the value to ensure it lies within -Pi ... +Pi */
    def normalize: Radians = Radians(value.wrap2(math.Pi))

    def - (that: Radians): Radians = Radians(this.value - that.value)
    def + (that: Radians): Radians = Radians(this.value + that.value)

    def angleTo(that: Radians): Radians = {
      val thisN = this.normalize
      val thatN = that.normalize
      (thatN - thisN).normalize
    }
  }

  /** Maps latitude index to longitude index to
    * a pair of dymaxion coordinate and northward direction ("compass")
    */
  final val LatLonIndicesToDymCompassMap: Map[LatLonIdx, (DymPt, Radians)] =
    LatLonIndicesToDymMap.map { case (idx, pt1) =>
      val latS = idx.latIdx - 1
      val latN = idx.latIdx + 1
      val ptS  = if (latS  <  0) DymPt(-99, -99) else LatLonIndicesToDymMap(LatLonIdx(latS, idx.lonIdx))
      val ptN  = if (latN >= 73) DymPt(-99, -99) else LatLonIndicesToDymMap(LatLonIdx(latN, idx.lonIdx))

      val pt1Eq     = pt1.equalize
      val ptSEq     = ptS.equalize
      val ptNEq     = ptN.equalize
      val useSouth  = (pt1Eq distanceTo ptSEq) < (pt1Eq distanceTo ptNEq)
      val pt2Eq     = if (useSouth) ptSEq else ptNEq
      val ang1      = math.atan2(-(pt2Eq.y - pt1Eq.y), pt2Eq.x - pt1Eq.x)
      val ang       = if (useSouth) (ang1 + math.Pi) % (2 * math.Pi) else ang1

      idx -> (pt1, Radians(ang))
    }

  /** Maps from loudspeaker channels to
    * quantized (lat, lon) pairs. Nearest
    * raster points are used except where
    * there are "jumps" in the Dymaxion,
    * in which case we go "away" a little
    * from the gap, so there are no two
    * loudspeakers with the same geo-coordinates.
    */
  final val ChannelToGeoMap: Map[Spk, LatLonIdx] = ChannelToMatrixMap.map { case (spk, dym) =>
    val pt1 = dym.toPoint.equalize
    val nn  = LatLonDym.minBy { case (ll, pt2) =>
      pt1 distanceTo pt2.equalize
    }
    spk -> nn._1
  }

  assert(ChannelToGeoMap.size == 42 && ChannelToGeoMap.valuesIterator.toSet.size == 42)

  // ChannelToGeoMap2.toList.sortBy(_._1.num).map(tup => (tup._1, tup._2.toLatLon)).foreach(println)

  final val VoronoiMap: Map[Spk, Vec[LatLonIdx]] = {
    val norm = ChannelToMatrixMap.map { case (spk, dym) => spk -> dym.toPoint }
    val v: Vec[(Spk, LatLonIdx)] = LatLonDym.map { case (latLonIdx, dym2) =>
      val pt2 = dym2.equalize
      val spk = norm.minBy(_._2.equalize.distanceTo(pt2))._1
      (spk, latLonIdx)
    }
    import kollflitz.Ops._
    v.toMultiMap(_._1)(_._2)
  }

  def channelCrosses(spk: Spk): Vec[(DymPt, Radians)] = VoronoiMap.getOrElse(spk, Vec.empty).map { idx =>
    LatLonIndicesToDymCompassMap(idx)
  }

  def decimate[A](in: Vec[A])(n: Int): Vec[A] = in.iterator.zipWithIndex.collect {
    case (x, i) if i % n == 0 => x
  } .toIndexedSeq

  // don't use `App` because body will not be initialized if we don't use it as main entry
  def main(args: Array[String]): Unit = {
    Main.main(args :+ "--mellite-frame")
    val pkg = "at.iem.sysson.turbulence._" :: Nil
    Code.registerImports(Code.Action    .id, pkg)
    Code.registerImports(Code.SynthGraph.id, pkg)
    atomic { implicit itx =>
      implicit val tx = Txn.wrap(itx)
      Sensors.init()
    }

    Swing.onEDT(initViews())
  }

  private def initViews(): Unit = {
    val dyn     = new DymaxionView
    //    dyn.drawImage    = false
    //    dyn.drawSpeakers = false
    // dyn.crosses = decimate(Preparations.dymGridAng.map(decimate(_)(8)))(4).flatten

    // dyn.crosses      = LatLonIndicesToDymCompassMap.valuesIterator.toVector
    dyn.crosses = channelCrosses(Spk(29)) ++ channelCrosses(Spk(6))
    dyn.mouseControl = false

    // val merc = new MercatorView(dyn)

    val fDyn = new Frame {
      contents = dyn
      resizable = false
    }

    new pdflitz.SaveAction(dyn :: Nil).setupMenu(fDyn)
    fDyn.pack()
    fDyn.centerOnScreen()
    fDyn.open()

    //    new Frame {
    //      contents = merc
    //      resizable = false
    //      pack()
    //      centerOnScreen()
    //      open()
    //    }

    new Frame {
      contents = new ToggleButton("Binaural Mix") {
        listenTo(this)
        reactions += {
          case ButtonClicked(_) => toggleBinaural(selected)
        }
      }
      pack()
      this.defaultCloseOperation = CloseOperation.Ignore
      open()
    }
  }

  private val binGroup = Ref(Option.empty[Group])

  private def toggleBinaural(on: Boolean): Unit = {
    atomic { implicit itx =>
      implicit val tx = Txn.wrap(itx)
      val listener = Binaural.Person(ChannelToMatrixMap(Spk(27)).toPoint, Radians(0.0))
      Mellite.auralSystem.serverOption.foreach { s =>
        binGroup.swap(None)(tx.peer).foreach(_.dispose())
        if (on) {
          val b = Binaural.build(s, listener)
          binGroup.set(Some(b))(tx.peer)
        }
      }
    }
  }

  // ---------------------------------

  def convertViaVoronoi(inF: File, varName: String, outF: File): Unit = {
    import sysson.Implicits._
    import TransformNetcdfFile.{Keep, Create}
    val in = openFile(inF)
    println(in.variableMap(varName).dimensions.map(_.name))
    try {
      val spkData = ma2.Array.factory(Turbulence.Channels.map(_.num)(breakOut): Array[Int])
      TransformNetcdfFile(in, outF, varName, Vec("lat", "lon"), Vec(Keep("time"), Create("spk", None, spkData))) {
        case (origin, arr) =>
          val dIn   = arr.copyToNDJavaArray().asInstanceOf[Array[Array[Float]]]
          val dOut: Array[Float] = Channels.map { spk =>
            val latLonIndices = VoronoiMap(spk)
            val sum = (0.0 /: latLonIndices) { case (n, idx) =>
              n + dIn(idx.latIdx)(idx.lonIdx)
            }
            val mean = sum / latLonIndices.size
            mean.toFloat
          } (breakOut)
          ma2.Array.factory(dOut)
      }
    } finally {
      in.close()
    }
  }
}

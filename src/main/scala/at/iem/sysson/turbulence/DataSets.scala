/*
 *  DataSets.scala
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
import de.sciss.file._
import de.sciss.lucre.geom.IntRectangle
import de.sciss.numbers
import de.sciss.synth.io.{AudioFileSpec, AudioFile}
import ucar.ma2

import scala.annotation.tailrec
import scala.collection._

object DataSets {
  def sysSonDir   = userHome / "IEM" / "SysSon"
  def dlrDir      = sysSonDir / "Data" / "201211" / "gcm" / "New_Deutschlandradio_MPI_M"
  def workshopDir = sysSonDir / "WorkshopSep2014"
  def dataOutDir  = sysSonDir / "installation" / "data"

  def main(args: Array[String]): Unit = args.headOption.getOrElse("") match {
    case "--pr-voronoi" =>
      val vr  = "pr"
      val inF = dlrDir / vr / s"25_${vr}_Amon_MPI-ESM-LR_historical_r1i1p1_185001-200512.nc"
      val outF = dataOutDir / s"${vr}_Amon_hist_voronoi.nc"
      convertViaVoronoi(inF = inF, varName = vr, outF = outF)

    case "--tas-voronoi" =>
      val vr  = "tas"
      val inF = dlrDir / vr / s"25_${vr}_Amon_MPI-ESM-LR_historical_r1i1p1_185001-200512.nc"
      val outF = dataOutDir / s"${vr}_Amon_hist_voronoi.nc"
      convertViaVoronoi(inF = inF, varName = vr, outF = outF)

    case "--ta-anomalies"  => calcTemperatureAnomalies(1)
    case "--ta-anomalies2" => calcTemperatureAnomalies(2)

    case "--precipitation-blobs" => calcPrecipitationBlobs()

    case other => sys.error(s"Unsupported command: $other")
  }

  // ----------- calcPrecipitationBlobs -----------

  def calcPrecipitationBlobs(): Unit = {
    val threshold = 2.0e-5
    val maxBlobs  = 10

    sealed trait State { def id: Int }
    case object Free  extends State { val id = 0 }
    case object Born  extends State { val id = 1 }
    case object Carry extends State { val id = 2 }

    // latIdx and lonIdx are "left-top" coordinates
    case class Blob(state: State, latIdx: Int, lonIdx: Int, latExtent: Int, lonExtent: Int,
                    sum: Double, max: Double) {
      def isFree = state == Free

      def toRectangle = IntRectangle(latIdx, lonIdx, latExtent, lonExtent)

      // because we don't have a `touch` method
      private def enlarge(r: IntRectangle) = r.copy(width = r.width + 1, height = r.height + 1)

      private def touch(a: IntRectangle, b: IntRectangle): Boolean =
        overlapArea(enlarge(a), enlarge(b)) > 0

      private def overlapArea(r1: IntRectangle, r2: IntRectangle): Long = {
        val l = math.max(r1.left , r2.left).toLong
        val r = math.min(r1.right, r2.right).toLong
        val w = r - l + 1
        if (w <= 0L) return 0L
        val t = math.max(r1.top   , r2.top   ).toLong
        val b = math.min(r1.bottom, r2.bottom).toLong
        val h = b - t + 1
        if (h <= 0L) return 0L
        w * h
      }

      private def area(r: IntRectangle) = overlapArea(r, r) // D'oh!

      // `true` if label can be interpreted as evolution of this blob
      def resembles(that: Blob): Boolean = {
        val thisR = this.toRectangle
        val thatR = that.toRectangle
        val thisA = area(thisR)
        val thatA = area(thatR)
        val change = math.max(thisA, thatA).toDouble / math.min(thisA, thatA) // growth or shrinkage in percent
        touch(thisR, thatR) && change <= 2.3456  // your favourite number here
      }
    }

    type Frame      = Vector[Blob]
    val freeBlob    = Blob(Free, 0, 0, 0, 0, 0.0, 0.0)
    val emptyFrame  = Vector.fill(maxBlobs)(freeBlob)

    import sysson.Implicits._

    val vrName  = "pr"
    val inF     = dlrDir / vrName / s"25_${vrName}_Amon_MPI-ESM-LR_historical_r1i1p1_185001-200512.nc"
    val in      = openFile(inF)
    val vr      = in.variableMap(vrName)
    val dims    = vr.dimensionMap
    val numTime = dims("time").size
    val numLats = dims("lat" ).size
    val numLons = dims("lon" ).size

    def analyze(prev: Frame, t: Int): Frame = {
      val sec   = vr in "time" select t
      val arr0  = sec.read()
      // [lat][lon]
      val arr     = arr0.reduce().copyToNDJavaArray().asInstanceOf[Array[Array[Float]]]
      // indices into the blob list we're building.
      val labelMap  = Array.ofDim[Int](numLats, numLons)
      var labels    = Vector.empty[Blob]

      for (lat <- 0 until numLats) {
        for (lon <- 0 until numLons) {
          val pr = arr(lat)(lon)
          if (pr < threshold) {
            labelMap(lat)(lon) = -1
          } else {
            val lbLeft  = if (lat == 0) -1 else labelMap(lat - 1)(lon    )
            val lbTop   = if (lon == 0) -1 else labelMap(lat    )(lon - 1)

            if (lbLeft >= 0) {         // blob to the left
              labelMap(lat)(lon) = lbLeft
              val lb0 = labels(lbLeft)
              val lb1 = lb0.copy(latExtent = lb0.latExtent + 1, sum = lb0.sum + pr,
                max = math.max(lb0.max, pr))
              labels = labels.updated(lbLeft, lb1)
              if (lbTop >= 0 && lbTop != lbLeft) {  // unite blobs
                val lb2   = labels(lbTop)
                val fLat  = math.min(lb1.latIdx, lb2.latIdx)
                val fLon  = math.min(lb1.lonIdx, lb2.lonIdx)
                val fLatE = math.max(lb1.latIdx + lb1.latExtent, lb2.latIdx + lb2.latExtent) - fLat
                val fLonE = math.max(lb1.lonIdx + lb1.lonExtent, lb2.lonIdx + lb2.lonExtent) - fLon
                // state is irrelevant here
                val lb3   = Blob(state = Free, latIdx = fLat, lonIdx = fLon, latExtent = fLatE, lonExtent = fLonE,
                  sum = lb1.sum + lb2.sum, max = math.max(lb1.max, lb2.max))
                // (1) we simply replace the left label,
                // then (2) we'll overwrite the top label map entries
                // with the left label index. then (3) we'll remove
                // the top label and (4) decrement all indices higher
                // than that.
                labels = labels.updated(lbLeft, lb3)  // (1)
                for (i <- 0 until numLats) {
                  for (j <- 0 until numLons) {
                    if (labelMap(i)(j) == lbTop) labelMap(i)(j) = lbLeft  // (2)
                  }
                }
                labels = labels.patch(lbTop, Nil, 1)  // (3)
                for (i <- 0 until numLats) {
                  for (j <- 0 until numLons) {
                    if (labelMap(i)(j) > lbTop) labelMap(i)(j) -= 1 // (4)
                  }
                }
              }

            } else if (lbTop >= 0) {  // blob to the right
              labelMap(lat)(lon) = lbTop
              val lb0 = labels(lbTop)
              val lb1 = lb0.copy(lonExtent = lb0.lonExtent + 1, sum = lb0.sum + pr,
                max = math.max(lb0.max, pr))
              labels = labels.updated(lbTop, lb1)

            } else {  // new blob
              labelMap(lat)(lon) = labels.size
              // state is irrelevant here
              labels :+= Blob(state = Free, latIdx = lat, lonIdx = lon, latExtent = 1, lonExtent = 1, sum = pr, max = pr)
            }
          }
        }
      }
      // ...now we have scanned the whole matrix...
      // we'll (1) sort the list by sum or max, (2) limit
      // the size to maxBlobs,
      // then we'll have to find connections to the
      // previous frame.

      val sorted = labels.sortBy(_.sum).take(maxBlobs)    // (1) (2)

      val (blob0, rem0) = ((emptyFrame, sorted) /: sorted) { case ((res, rem), lb) =>
        val carryIdx = (prev zip res).indexWhere {
          case (p, x) => x.isFree && p.resembles(lb)
        }
        // no fusion? keep label in remaining
        if (carryIdx < 0) (res, rem :+ lb)
        else (res.updated(carryIdx, lb.copy(state = Carry)), rem)
      }

      val next = (blob0 /: rem0) { case (res, lb) =>
        res.updated(res.indexWhere(_.isFree), lb.copy(state = Born))
      }

      next
    }

    val outF  = dataOutDir / s"pr_amon_hist_blob.nc"
    val spkData = ma2.Array.factory(Turbulence.Channels.map(_.num)(breakOut): Array[Int])
//    TransformNetcdfFile(in, outF, varName, Vec("lat", "lon"), Vec(Keep("time"), Create("spk", None, spkData))) {
//
//    }

    ???

    @tailrec def loop(prev: Frame, t: Int): Unit = if (t < numTime) {
      val next = analyze(prev, t)

      loop(next, t + 1)
    }

    loop(emptyFrame, t = 0)
    in.close()
  }

  // ----------- calcTemperatureAnomalies -----------

  def calcTemperatureAnomalies(version: Int): Unit = {
    import Turbulence._
    import Implicits._
    import numbers.Implicits._

    val in        = openFile(workshopDir / "RO_Temperatures.nc")
    val varName   = "Temperature"
    val presName  = "Pressure"
    val latName   = "Latitude"
    val timeName  = "Time"
    val pressures = in.variableMap(presName).read().double1D
    val latitudes = in.variableMap(latName ).read().float1D
    val nc        = in.variableMap(varName)
    val numTime   = nc.dimensionMap(timeName).size
    val p0 = version match {
      case 1 => 100.0
      case 2 =>  70.0
    }
    val presIdx0  = pressures.indexOf(p0)    // was: 100 hPa to 10 hPa

    val grid = Channels.map { spk =>
      val ptd     = ChannelToMatrixMap(spk).toPoint
      val pt      = ptd.equalize
      val lat     = LatLonDym.minBy(_._2.equalize distanceTo pt)._1.toLatLon.lat
      val latIdx  = latitudes.indexOf(latitudes.minBy(_ absdif lat))
      val presIdx = presIdx0 + ptd.y.toInt
      (latIdx, presIdx)
    }

    def calcNorm(month: Int): Vec[Float] = {
      // Note: the first three months of the data set are
      // invalid (contain fill-values / NaNs). Also, for
      // certain pressure levels, fill values occur.
      // We remove them by selecting only values less than 1.0e10.
      // If at a particular pres/lat no single valid value is
      // found, we return zero.

      val sel0 = nc in timeName select (month until numTime by 12)

      def calc(presIdx: Int, latIdx: Int): Float = {
        val sel1  = sel0 in presName select presIdx
        val sel2  = sel1 in latName  select latIdx
        val all   = sel2.read().double1D
        val valid = all.filter(_ < 1.0e10) // without fill-values / NaNs !
        if (valid.size == 0) {
          println(s"No valid values for pres ${pressures(presIdx)} / lat ${latitudes(latIdx)}!")
          0f
        } else {
          (valid.sum / valid.size).toFloat
        }
      }

      grid.map { case (latIdx, presIdx) => calc(presIdx = presIdx, latIdx = latIdx) }
    }

    val norm: Vec[Vec[Float]] = Vec.tabulate(12)(calcNorm)

    val outF = dataOutDir / s"ta_anom_spk${if (version == 1) "" else version.toString}.nc"

    import TransformNetcdfFile.{Keep, Create}
    val spkData = ma2.Array.factory(Turbulence.Channels.map(_.num)(breakOut): Array[Int])
    TransformNetcdfFile(in, outF, varName, Vec(latName, presName), Vec(Keep(timeName), Create("spk", None, spkData))) {
      case (origin, arr) =>
        val dIn = arr.copyToNDJavaArray().asInstanceOf[Array[Array[Double]]]
        val dOut: Array[Float] = grid.zipWithIndex.map { case ((latIdx, presIdx), gi) =>
          val ta    = dIn(latIdx)(presIdx).toFloat
          val month = origin(0) % 12
          val isNaN = ta >= 1.0e10
          val anom  = ta - norm(month)(gi)
          if (isNaN) Float.NaN else anom
        } (breakOut)
        ma2.Array.factory(dOut)
    }

    in.close()
  }

  // ----------- convertViaVoronoi -----------

  def convertViaVoronoi(inF: File, varName: String, outF: File): Unit = {
    import Implicits._
    import TransformNetcdfFile.{Keep, Create}
    val in = openFile(inF)
    println(in.variableMap(varName).dimensions.map(_.name))
    try {
      val spkData = ma2.Array.factory(Turbulence.Channels.map(_.num)(breakOut): Array[Int])
      TransformNetcdfFile(in, outF, varName, Vec("lat", "lon"), Vec(Keep("time"), Create("spk", None, spkData))) {
        case (origin, arr) =>
          val dIn = arr.copyToNDJavaArray().asInstanceOf[Array[Array[Float]]]
          val dOut: Array[Float] = Turbulence.Channels.map { spk =>
            val latLonIndices = Turbulence.VoronoiMap(spk)
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
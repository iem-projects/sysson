/*
 *  DataSets.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 Hanns Holger Rutz. All rights reserved.
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
import at.iem.sysson.turbulence.Dymaxion.DymPt
import at.iem.sysson.turbulence.TransformNetcdfFile.{Create, Keep}
import at.iem.sysson.turbulence.Turbulence.{DymGrid, LatLonIdx, LatLon}
import de.sciss.file._
import de.sciss.lucre.geom.IntRectangle
import de.sciss.numbers
import de.sciss.synth.io.{AudioFileSpec, AudioFile}
import ucar.ma2

import scala.collection.breakOut
import scala.concurrent.duration.Duration
import scala.concurrent.{Future, Await, ExecutionContext}
import scala.concurrent.stm.TxnExecutor
import scala.util.{Failure, Success}

object DataSets {
  def sysSonDir   = userHome  / "IEM" / "SysSon"
  def dlrDir      = sysSonDir / "Data" / "201211" / "gcm" / "New_Deutschlandradio_MPI_M"
  def workshopDir = sysSonDir / "WorkshopSep2014"
  def dataOutDir  = sysSonDir / "installation" / "data"

  private def join(vr: String, outF: File): Unit = {
    val in1F  = dlrDir / vr / s"25_${vr}_Amon_MPI-ESM-LR_historical_r1i1p1_185001-200512.nc"
    val in2F  = dlrDir / vr / s"25_${vr}_Amon_MPI-ESM-LR_rcp45_r1i1p1_200601-230012.nc"
    val in1   = openFile(in1F)
    val in2   = openFile(in2F)
    TransformNetcdfFile.concat(in1, in2, outF, vr)
    in1.close(); in2.close()
  }

  private def mkVoronoi(vr: String): Unit = {
    val outFJ = File.createTemp("turbulence", ".nc")
    join(vr, outFJ)
    val outF  = dataOutDir / s"${vr}_amon_join_voronoi.nc"
    convertViaVoronoi(inF = outFJ, varName = vr, outF = outF)
  }

  def main(args: Array[String]): Unit = args.headOption.getOrElse("") match {
    case "--pr-voronoi"   => mkVoronoi("pr")
    case "--tas-voronoi"  => mkVoronoi("tas")

    case "--ta-anomalies"         => calcTemperatureAnomalies(1)
    case "--ta-anomalies2"        => calcTemperatureAnomalies(2)
    case "--precipitation-blobs"  => calcPrecipitationBlobs()
    case "--dymgrid-chan-map"     => calcDymGridChans()
    case "--glue"                 => createGluedFiles()
    case "--wind"                 => createWindFiles()

    case other => sys.error(s"Unsupported command: $other")
  }

  // ----------- createWindFiles -----------

  def createWindFiles(): Unit = {
    val vrName = "ua"

    val in1F  = dlrDir / vrName / s"ZON_200hPa_${vrName}_Amon_MPI-ESM-LR_historical_r1i1p1_185001-200512.nc"
    val in2F  = dlrDir / vrName / s"ZON_200hPa_${vrName}_Amon_MPI-ESM-LR_rcp45_r1i1p1_200601-230012.nc"
    val outF  = dataOutDir / s"ZON_200hPa_${vrName}_amon_join.nc"
    val in1   = openFile(in1F)
    val in2   = openFile(in2F)
    TransformNetcdfFile.concat(in1, in2, outF, vrName)
  }

  // ----------- createGluedFiles -----------

  def createGluedFiles(): Unit = {
    val vrs   = Seq("hfls", "hfss", "rlds", "rlus", "rlut", "rsds", "rsdt", "rsus", "rsut", "rtmt")

    import ExecutionContext.Implicits.global
    val futs = vrs.map { name =>
      println(s"Joining $name...")
      val in1F  = dlrDir / "radiation" / s"avg_${name}_Amon_MPI-ESM-LR_historical_r1i1p1_185001-200512.nc"
      val in2F  = dlrDir / "radiation" / s"avg_${name}_Amon_MPI-ESM-LR_rcp45_r1i1p1_200601-230012.nc"
      val outF  = dataOutDir / s"avg_${name}_amon_join.nc"
      val in1   = openFile(in1F)
      val in2   = openFile(in2F)
      TransformNetcdfFile.concat(in1, in2, outF, name)
      val in3   = openFile(outF)
      val stats = TxnExecutor.defaultAtomic { implicit tx => Stats.get(in3) }
      stats.onComplete { tr =>
        in3.close()
        tr match {
          case Success(s) =>
            println(s"======== stats for '$name' ========")
            println(s(name).total)
          case Failure(e) =>
            println(s"Failed to read file $outF")
            e.printStackTrace()
        }
      }
      in1.close()
      in2.close()
      stats
    }
    Await.ready(Future.sequence(futs), Duration.Inf)
    println("Done.")
  }

  // ----------- calcDymChans -----------

  def calcDymGridChans(): Unit = {
    val outF  = dataOutDir / "dymgrid-chan-map.aif"
    val out   = AudioFile.openWrite(outF, AudioFileSpec(numChannels = 1, sampleRate = 44100))
    val buf   = for {
      x <- 0 until 14
      y <- 0 until  5
    } yield {
      val idx = Turbulence.MatrixToChannelMap.get(DymGrid(vx = x, vyi = y))
        .map(Turbulence.Channels.indexOf).getOrElse(-1)
      idx.toFloat
    }
    out.write(Array(buf.toArray))
    out.close()
  }

  // ----------- calcPrecipitationBlobs -----------

  final val PrecipBlobThreshold = 1.0e-6

  def calcPrecipitationBlobs(): Unit = {
    val threshold = PrecipBlobThreshold // 1.0e-5
    import MakeLayers.NumBlobs

    sealed trait State {
      def isFree: Boolean
      def toAudioFrame(prev: State): Array[Float]
    }
    case object Free extends State {
      def isFree = true

      def toAudioFrame(prev: State): Array[Float] = {
        prev match {
          case Free => Array(0f, 0f, 0f, PrecipBlobThreshold.toFloat, PrecipBlobThreshold.toFloat)
          case b: Blob =>
            // yo, a bit dirty. repeat last frame and set gate to zero
            val arr = b.toAudioFrame(Free)
            arr(0)  = 0f
            arr
        }
      }
    }

    // latIdx and lonIdx are "left-top" coordinates
    case class Blob(latIdx: Int, lonIdx: Int, latExtent: Int, lonExtent: Int,
                    sum: Double, max: Double) extends State {
      def isFree = false

      def toRectangle = IntRectangle(latIdx, lonIdx, latExtent, lonExtent)

      def area: Long = area(toRectangle)

      def toDym: DymPt = {
        val ll1 = LatLonIdx(latIdx = latIdx, lonIdx = lonIdx).toLatLon
        val ll2 = LatLonIdx(latIdx = latIdx + latExtent - 1, lonIdx = lonIdx + lonExtent - 1).toLatLon
        val ll3 = LatLon(lat = (ll1.lat + ll2.lat)/2, lon = (ll1.lon + ll2.lon)/2) // blob center
        Dymaxion.mapLonLat(ll3)
      }

      def toAudioFrame(prev: State): Array[Float] = {
        val dymPt = toDym
        assert(sum >= PrecipBlobThreshold)
        assert(max >= PrecipBlobThreshold)
        Array(1f, dymPt.x.toFloat, dymPt.y.toFloat, sum.toFloat, max.toFloat)
      }

      // because we don't have a `touch` method
      private def enlarge(r: IntRectangle, amt: Int) =
        r.copy(width = r.width + amt, height = r.height + amt)

      private def touch(a: IntRectangle, b: IntRectangle): Boolean =
        overlapArea(enlarge(a, math.max(1, a.width * 2 / 3)), enlarge(b, math.max(1, b.width * 2 / 3))) > 0

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
        val mercator  = touch(thisR, thatR) && change <= 4 // 2.3456  // your favourite number here
        val dym       = (this.toDym.equalize distanceTo that.toDym.equalize) <= Dymaxion.hScale * 2

        mercator && dym // what do you want more?
      }
    }

    type Frame      = Vec[State]
    val numVoices   = 2 * NumBlobs
    val emptyFrame  = Vec.fill[State](numVoices)(Free)

    import sysson.Implicits._

    val vrName  = "pr"
    val outFJ = File.createTemp("turbulence", ".nc")
    join(vrName, outFJ)
    val inF     = outFJ

    val in      = openFile(inF)
    val vr      = in.variableMap(vrName)
    val dims    = vr.dimensionMap
    // val numTime = dims("time").size
    val numLats = dims("lat" ).size
    val numLons = dims("lon" ).size

    def analyze(prev: Frame, arr0: ma2.Array): Frame = {
      // [lat][lon]
      val arr     = arr0.reduce().copyToNDJavaArray().asInstanceOf[Array[Array[Float]]]
      // indices into the blob list we're building.
      val labelMap  = Array.ofDim[Int](numLats, numLons)
      var labels    = Vec.empty[Blob]

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
                val lb3   = Blob(latIdx = fLat, lonIdx = fLon, latExtent = fLatE, lonExtent = fLonE,
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
              labels :+= Blob(latIdx = lat, lonIdx = lon, latExtent = 1, lonExtent = 1, sum = pr, max = pr)
            }
          }
        }
      }
      // ...now we have scanned the whole matrix...
      // we'll (1) sort the list by sum or max, (2) limit
      // the size to maxBlobs,
      // then we'll have to find connections to the
      // previous frame.

      val sorted0 = labels.sortBy(lb => (lb.area, lb.max))   // (1)

      val sorted = sorted0.take(NumBlobs)  // (2)

      // first try to find contiguous blobs
      val (blob0, rem0) = ((Vec.empty[State], sorted) /: prev) { case ((res, rem), prevElem) =>
        val fuseOpt = prevElem match {
          case b: Blob => rem.find(_ resembles b)
          case _ => None
        }
        val (n, rem1) = fuseOpt.fold[(State, Vec[Blob])]((Free, rem))(p => (p, rem.filterNot(_ == p)))

        (res :+ n, rem1)
      }

      //      val (blob0, rem0) = ((emptyFrame, Vector.empty[Blob]) /: sorted) { case ((res, rem), lb) =>
      //        val carryIdx = (prev zip res).indexWhere {
      //          case (p: Blob, x) => x.isFree && p.resembles(lb)
      //          case _ => false
      //        }
      //        // no fusion? keep label in remaining
      //        if (carryIdx < 0) (res, rem :+ lb)
      //        else (res.updated(carryIdx, lb /* .copy(state = Carry) */), rem)
      //      }

      val numFree = (blob0 zip prev).count { case (n, p) => n.isFree && p.isFree }

      val next = (blob0 /: rem0.take(numFree)) { case (res, lb) =>
        val idx = (res zip prev).indexWhere { case (n, p) => n.isFree && p.isFree }
        res.updated(idx, lb /* .copy(state = Born) */)
      }

      next
    }

    val outF      = dataOutDir / s"pr_amon_join_blob.nc"
    val blobData  = ma2.Array.factory((0 until (5 * numVoices)).toArray)

    var prevFrame = emptyFrame  // XXX not cool, TransformNetcdfFile doesn't thread state
    var maxSum    = 0.0
    var maxMax    = 0.0
    var totalCont = 0
    var totalFree = 0

    // [state, cx, cy, sum, max] x maxBlobs

    TransformNetcdfFile(in, outF, vrName, Vec("lat", "lon"), Vec(Keep("time"), Create("blob", None, blobData))) {
      case (origin, arr) =>
        val nextFrame = analyze(prevFrame, arr)

        val numCont = (nextFrame zip prevFrame).count { case (n, p) => !n.isFree && !p.isFree }
        val numFree = nextFrame.count(_.isFree)
        totalCont += numCont
        totalFree += numFree
        //  println(s"Blobs: ${nextFrame.count(!_.isFree)}. Contiguous = $numCont")

        val dOut: Array[Float] = (nextFrame zip prevFrame).flatMap { case (b, bPrev) =>
          val arr   = b.toAudioFrame(bPrev)
          require(arr.length == 5)
          val bSum  = arr(3)
          val bMax  = arr(4)
          maxSum    = math.max(maxSum, bSum) // yeah, I know...
          maxMax    = math.max(maxMax, bMax) // yeah, I know...
          arr
        } (breakOut)

        prevFrame     = nextFrame // yeah, I know...

        ma2.Array.factory(dOut)
    }

    println(s"maxSum = $maxSum, maxMax = $maxMax")
    println(s"Total number of contiguous blobs: $totalCont; total free: $totalFree")

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
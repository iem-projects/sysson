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

import de.sciss.file._
import de.sciss.numbers
import ucar.ma2

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

    case other => sys.error(s"Unsupported command: $other")
  }

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
    val latitudes = in.variableMap(latName).read().float1D
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
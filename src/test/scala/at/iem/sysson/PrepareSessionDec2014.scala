//package at.iem.sysson
//
//import at.iem.sysson.util.NetCdfFileUtil
//import de.sciss.file._
//import ucar.ma2
//
//object PrepareSessionDec2014 extends App {
//  calcTemperatureAnomalies(varName = "pr" )
//  calcTemperatureAnomalies(varName = "tas")
//
//  def calcTemperatureAnomalies(varName: String): Unit = {
//    import Implicits._
//
//    val dataOutDir = dataDir
//    val outF = dataOutDir / s"${varName}_fut_anom.nc"
//
//    if (outF.exists()) {
//      println(s"File '$outF' already exists. Not overwriting.")
//      return
//    }
//
//    val inF       = dataDir / "201211" / "gcm" / "New_Deutschlandradio_MPI_M" / varName /
//      s"25_${varName}_Amon_MPI-ESM-LR_rcp45_r1i1p1_200601-230012.nc"
//
//    val in          = openFile(inF)
//    val lonName     = "lon"
//    val latName     = "lat"
//    val timeName    = "time"
//    val lonVar      = in.variableMap(lonName)
//    val lonData     = lonVar.read()
//    val longitudes  = lonData.double1D
//    val numLon      = in.dimensionMap(lonName).size
//    val latVar      = in.variableMap(latName)
//    val latData     = latVar.read()
//    val latitudes   = latData.double1D
//    val numLat      = in.dimensionMap(latName).size
//    val nc          = in.variableMap(varName)
//    val numTime     = nc.dimensionMap(timeName).size
//
//    def calcNorm(month: Int, yearLo: Int, yearHi: Int): Vec[Vec[Float]] = {
//      // Note: the first three months of the data set are
//      // invalid (contain fill-values / NaNs). Also, for
//      // certain pressure levels, fill values occur.
//      // We remove them by selecting only values less than 1.0e10.
//      // If at a particular pres/lat no single valid value is
//      // found, we return zero.
//
//      val sel0 = nc in timeName select ((yearLo * 12 + month) until (yearHi * 12 + month) by 12)
//
//      def calc(lonIdx: Int, latIdx: Int): Float = {
//        val sel1  = sel0 in lonName select lonIdx
//        val sel2  = sel1 in latName select latIdx
//        val all   = sel2.read().float1D // double1D
//        val valid = all // .filter(_ < 1.0e10) // without fill-values / NaNs !
//        if (valid.size == 0) {
//          println(s"No valid values for pres ${longitudes(lonIdx)} / lat ${latitudes(latIdx)}!")
//          0f
//        } else valid.sum / valid.size
//      }
//
//      (0 until numLat).map { latIdx =>
//        (0 until numLon).map { lonIdx =>
//          calc(latIdx = latIdx, lonIdx = lonIdx)
//        }
//      }
//    }
//
//    val winLen  = 30 // in years
//    val winLenH = winLen / 2
//
//    // val norm: Vec[Vec[Vec[Float]]] = Vec.tabulate(12)(calcNorm)
//
//    import NetCdfFileUtil.{Create, Keep}
//
//    println(s"Heavily crunching data for ${outF.name}...")
//
//    NetCdfFileUtil.transform(in = in, out = outF, varName = varName, inDims = Vec(latName, lonName),
//      outDimsSpec = Vec(Keep(timeName), Create(latName, latVar.units, latData), Create(lonName, lonVar.units, lonData))) {
//      case (origin, arr) =>
//        val dIn     = arr.copyToNDJavaArray().asInstanceOf[Array[Array[Float]]]
//        val time    = origin.head
//        println(s"Time = $time / $numTime")
//        val month   = time % 12
//        val year    = time / 12
//        val yearLo  = (year - winLenH).max(0)
//        val yearHi  = (year + winLenH).min(numTime / 12)
//        val norm    = calcNorm(month = month, yearLo = yearLo, yearHi = yearHi)
//
//        val dOut = Array.tabulate[Array[Float]](dIn.length) { latIdx =>
//          Array.tabulate[Float](dIn(0).length) { lonIdx =>
//            val ta    = dIn(latIdx)(lonIdx)
//            val anom  = ta - norm(latIdx)(lonIdx)
//            anom
//          }
//        }
//        ma2.Array.factory(dOut)
//    }
//
//    in.close()
//    println("Done.")
//  }
//}

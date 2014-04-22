package at.iem.sysson

import Implicits._
import de.sciss.file._

object Session130403 extends SessionLike {
  lazy val supercollider  = true   // if `true`, print array data for sclang
  lazy val useMax         = false

  override lazy val useAudio = !supercollider

  // specific humidity !
  override lazy val dataName  = "25_hus_Amon_HadGEM2-ES_rcp45_r1i1p1_200512-210012.nc"
  override lazy val dataFile  = dataDir / "201211" / "gcm" / "RCP45" / "MetOffUK_HadGEM2-ES" / dataName

  lazy val time = 0
  lazy val plev = 5

  def run(): Unit = {
    // Met office hus 25, variable "hus"
    val v    = f.variableMap("hus")
    val sect = (v in "time" select time) in "plev" select plev
    assert(sect.reducedRank == 2)
    val arr  = sect.read()
    val red  = arr.reduce
    // j(latIdx)(longIdx)
    val j    = red.copyToNDJavaArray.asInstanceOf[Array[Array[Float]]]

    // reduction of 144 longitudes to 9 averaged groups
    // (replaces original array)
    for(lat <- 0 until 72) {
      val lon = j(lat)
      j(lat) = lon.sliding(16,16).toArray.map { arr =>
        if (useMax) arr.max else arr.sum / 16
      }
    }

    def lonSlice(l: Int) = j.map(_.apply(l))

    def latMax(l: Int) = {
      val slice = lonSlice(l)
      val mx = slice.max
      slice.indexOf(mx) -> mx
    }

    val lm = (0 until 9).map(latMax)

    def bandwidth(l: Int) = {
      val (lmi, lmv)  = lm(l)
      val thresh      = lmv * 0.7
      val slice       = lonSlice(l)
      val low         = slice.take(lmi)
      val lowRange    = low.reverse.indexWhere(_ < thresh) + 1
      val high        = slice.drop(lmi + 1)
      val highRange   = high.indexWhere(_ < thresh) + 1
      lowRange + highRange - 1
    }

    val lb = (0 until 9).map(bandwidth)

    val together = (lm zip lb).map { case ((a, b), c) => List(a,b,c) }
    val scCode = s"~lm = ${together.map(l => l.mkString("[", ",", "]")).mkString("[", ",", "]")}"

    val min = red.float1D.min // 5.2230465E-5
    val max = red.float1D.max // 0.0039939615

    if (supercollider) {
      println(s"At time index $time and plev index $plev:")
      println(s"~min = $min; ~max = $max;")
      println(scCode)
    } else {
      synth()
    }
  }

  def synth(): Unit = ???
}
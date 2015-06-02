package at.iem.sysson

import at.iem.sysson.turbulence.Turbulence
import at.iem.sysson.util.NetCdfFileUtil
import de.sciss.file._
import ucar.ma2

import scala.collection._

object NetCdfFileUtilTest {
  def main(args: Array[String]): Unit = {
    testOutput()
  }

  def testOutput(): Unit = {
    import NetCdfFileUtil._

    val inF = userHome / "IEM" / "SysSon" / "Data" / "201211" / "gcm" / "New_Deutschlandradio_MPI_M" /
      "tas" / "ZON_tas_Amon_MPI-ESM-LR_historical_r1i1p1_185001-200512.nc"
    val in = openFile(inF)
    try {
      val out = userHome / "Documents" / "temp" / "test.nc"
      val spkData = ma2.Array.factory(Turbulence.Channels.map(_.num)(breakOut): Array[Int])
      transform(in, out, "tas", Vec("lat", "lon"), Vec(Keep("time"), Create("spk", units = None, spkData))) {
        case (origin, arr) =>
          val dIn0  = arr.copyToNDJavaArray().asInstanceOf[Array[Array[Float]]]
          val dIn   = dIn0.flatten
          val dOut = Array.tabulate(Turbulence.Channels.size) { i =>
            (i + origin.head).toFloat * dIn(i % dIn.length)
          }
          ma2.Array.factory(dOut)
      }
    } finally {
      in.close()
    }
  }
}

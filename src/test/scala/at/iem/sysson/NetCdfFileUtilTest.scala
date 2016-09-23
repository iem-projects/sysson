package at.iem.sysson

import at.iem.sysson.util.NetCdfFileUtil
import de.sciss.file._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/** This example shows how to transform
  * a file along one of the variable's dimensions
  * (here "Altitude").
  */
object NetCdfFileUtilTest {
  def main(args: Array[String]): Unit = testOutput()

  def testOutput(): Unit = {
    import NetCdfFileUtil._

    val dir = userHome / "sysson" / "nc"
    val inF = dir / "5x30-climatology_2001-05-01_2016-05-01_RO_OPSv5.6.2_L2b_no_METOP_no_TerraSAR-X.nc"
    val in  = openFile(inF)
    try {
      import Implicits._
      val out     = dir / "test.nc"
      val altData = in.variableMap("Altitude").readSafe()
      val proc = transform(in, out, "Temperature", inDims = Vector("Altitude"),
        outDimsSpec = Vec(Keep("Time"), Keep("Longitude"), Keep("Latitude"),
          Create("Altitude", units = Some("m"), altData))) {

        case (origin, arr) =>
          // println(s"transform ${arr.size}")
          arr
      }
      import scala.concurrent.ExecutionContext.Implicits.global
      proc.start()
      Await.result(proc, Duration.Inf)

    } finally {
      in.close()
    }
  }
}

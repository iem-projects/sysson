package at.iem.sysson.turbulence

import de.sciss.file._

object DataSets {
  def main(args: Array[String]): Unit = args.headOption.getOrElse("") match {
    case "--precipitation-voronoi" =>
      val inF = userHome / "IEM" / "SysSon" / "Data" / "201211" / "gcm" / "New_Deutschlandradio_MPI_M" /
        "pr" / "25_pr_Amon_MPI-ESM-LR_historical_r1i1p1_185001-200512.nc"
      val outF = userHome / "Documents" / "temp" / "test_pr.nc"
      Turbulence.convertViaVoronoi(inF = inF, varName = "pr", outF = outF)
    case other => sys.error(s"Unsupported command: $other")
  }
}

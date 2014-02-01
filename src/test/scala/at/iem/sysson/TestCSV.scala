package at.iem.sysson

import de.sciss.file._

object TestCSV extends App {
  import Implicits._
  val f = open("/Users/hhrutz/Desktop/IEM/SysSon/data/201211/gcm/New_Deutschlandradio_MPI_M/tas/ZON_tas_Amon_MPI-ESM-LR_historical_r1i1p1_185001-200512.nc")
  f.exportAsCSV(file("/Users/hhrutz/Desktop/test.csv"))
}
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

package at.iem.sysson.turbulence

import de.sciss.file._

object DataSets {
  def sysSonDir = userHome / "IEM" / "SysSon"

  def dlrDir = sysSonDir / "Data" / "201211" / "gcm" / "New_Deutschlandradio_MPI_M"

  def dataOutDir = sysSonDir / "installation" / "data"

  def main(args: Array[String]): Unit = args.headOption.getOrElse("") match {
    case "--precipitation-voronoi" =>
      val inF = dlrDir / "pr" / "25_pr_Amon_MPI-ESM-LR_historical_r1i1p1_185001-200512.nc"
      val outF = dataOutDir / "pr_Amon_hist_voronoi.nc"
      Turbulence.convertViaVoronoi(inF = inF, varName = "pr", outF = outF)

    case "--precipitation-voronoi" =>
      val sysSonDir = userHome / "IEM" / "SysSon"
      val inF = sysSonDir / "Data" / "201211" / "gcm" / "New_Deutschlandradio_MPI_M" /
        "pr" / "25_pr_Amon_MPI-ESM-LR_historical_r1i1p1_185001-200512.nc"
      val outF = dataOutDir / "pr_Amon_hist_voronoi.nc"
      Turbulence.convertViaVoronoi(inF = inF, varName = "pr", outF = outF)

    case other => sys.error(s"Unsupported command: $other")
  }
}

package at.iem

import ucar.nc2
import de.sciss.synth
import java.io.File

package object sysson {
  def defaultPath = "/Users/hhrutz/Desktop/IEM/SysSon/netcdf/data/RO-MultiSatelliteClimatologies-SEremoved_plevData_months_012002-122010.nc"
  def openDefault() : nc2.NetcdfFile = open(defaultPath)

  def open(path: String): nc2.NetcdfFile = nc2.NetcdfFile.open(path).setImmutable()
  def openFile(file: File): nc2.NetcdfFile = open(file.getPath)

  def file(path: String): File = new File(path)

  def boot() {
    synth.Server.boot() {
      case _ => ()   // don't do anything specific now
    }
  }

  def logInfo(what: => String) {
    println("[info] " + what)
  }

  def logWarn(what: => String) {
    println("[warn] " + what)
  }

  type Scale = Double => Double
}

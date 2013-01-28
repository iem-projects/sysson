package at.iem

import java.io.File
import ucar.nc2
import de.sciss.synth

package object sysson {
  def defaultPath = new File("../netcdf/data/RO-MultiSatelliteClimatologies-SEremoved_plevData_months_012002-122010.nc").getAbsolutePath
  def openDefault() : nc2.NetcdfFile = nc2.NetcdfFile.open(defaultPath).setImmutable()
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
}

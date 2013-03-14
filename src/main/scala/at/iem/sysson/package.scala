package at.iem

import ucar.nc2
import de.sciss.synth
import java.io.File

package object sysson {
  import Implicits._

  /** The SysSon base directory is determined by the environment variable `SYSSON_HOME`. See the
    * `README.md` for more information.
    */
  val syssonDir   = sys.env.get("SYSSON_HOME") match {
    case Some(path) => file(path)
    case _ =>
      Console.err.println("WARNING: Environment variable SYSSON not set. Cannot access default data files.")
      val res = file(sys.props("user.home")) / "sysson"
      if (!res.isDirectory) res.mkdir()
      res
  }
  /** The `data` directory inside the SysSon base directory contains common NetCDF files. */
  val dataDir       = syssonDir / "data"

  def defaultFile   = dataDir / "RO-MultiSatelliteClimatologies-SEremoved_plevData_months_012002-122010.nc"
  def defaultFile2  = dataDir / "201211" / "gcm" / "RCP45" /
    "MetOffUK_HadGEM2-ES/25_ta_Amon_HadGEM2-ES_rcp45_r1i1p1_200512-210012.nc"

  /** Opens `RO-MultiSatelliteClimatologies-SEremoved_plevData_months_012002-122010.nc` */
  def openDefault (): nc2.NetcdfFile = openFile(defaultFile )
  /** Opens `MetOffUK_HadGEM2-ES/25_ta_Amon_HadGEM2-ES_rcp45_r1i1p1_200512-210012.nc` */
  def openDefault2(): nc2.NetcdfFile = openFile(defaultFile2)

  /** Opens a NetCDF file from a given path string. */
  def open(path: String): nc2.NetcdfFile = nc2.NetcdfFile.open(path).setImmutable()
  /** Opens a NetCDF file from a given file object . */
  def openFile(file: File): nc2.NetcdfFile = open(file.getPath)

  /** Convenient file object constructor from a given path string. */
  def file(path: String): File = new File(path)

  /** Boots the SuperCollider server. */
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

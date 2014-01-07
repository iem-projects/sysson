/*
 *  package.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem

import ucar.nc2
import de.sciss.synth
import de.sciss.file._

package object sysson {
  val  Vec     = collection.immutable.IndexedSeq
  type Vec[+A] = collection.immutable.IndexedSeq[A]

  /** The SysSon base directory is determined by the environment variable `SYSSON_HOME`. See the
    * `README.md` for more information.
    */
  val syssonDir   = sys.env.get("SYSSON_HOME") match {
    case Some(path) => file(path)
    case _ =>
      Console.err.println("WARNING: Environment variable SYSSON_HOME not set. Cannot access default data files.")
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
  def boot(): Unit =
    synth.Server.boot() {
      case _ => ()   // don't do anything specific now
    }

  def logInfo(what: => String): Unit = println("[info] " + what)

  def logWarn(what: => String): Unit = println("[warn] " + what)

  type Scale = Double => Double
}

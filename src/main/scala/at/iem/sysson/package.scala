/*
 *  package.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import at.iem.sysson.fscape.GenViewFactory
import at.iem.sysson.sound.{AuralSonification, Sonification}
import at.iem.sysson.util.DoubleTransform
import de.sciss.file._
import de.sciss.fscape.lucre.{FScape => LFScape}
import de.sciss.fscape.stream.Control
import de.sciss.lucre.matrix
import de.sciss.lucre.stm.TxnLike
import de.sciss.mellite.Mellite
import de.sciss.synth
import de.sciss.synth.proc.Code
import ucar.nc2

import scala.annotation.elidable
import scala.annotation.elidable.{CONFIG, INFO, WARNING}
import scala.concurrent.stm.Txn

package object sysson {
  val  Vec     = collection.immutable.IndexedSeq
  type Vec[+A] = collection.immutable.IndexedSeq[A]

  /** The SysSon base directory is determined by the environment variable `SYSSON_HOME`. See the
    * `README.md` for more information.
    */
  val syssonDir: File = sys.env.get("SYSSON_HOME") match {
    case Some(path) => file(path)
    case _ =>
      // Console.err.println("WARNING: Environment variable SYSSON_HOME not set. Cannot access default data files.")
      val res = file(sys.props("user.home")) / "sysson"
      if (!res.isDirectory) res.mkdir()
      res
  }
  /** The `data` directory inside the SysSon base directory contains common NetCDF files. */
  val dataDir: File = syssonDir / "data"

  def defaultFile : File  = dataDir / "RO-MultiSatelliteClimatologies-SEremoved_plevData_months_012002-122010.nc"
  def defaultFile2: File  = dataDir / "201211" / "gcm" / "RCP45" /
    "MetOffUK_HadGEM2-ES/25_ta_Amon_HadGEM2-ES_rcp45_r1i1p1_200512-210012.nc"

  /** Opens `RO-MultiSatelliteClimatologies-SEremoved_plevData_months_012002-122010.nc` */
  def openDefault (): nc2.NetcdfFile = openFile(defaultFile )
  /** Opens `MetOffUK_HadGEM2-ES/25_ta_Amon_HadGEM2-ES_rcp45_r1i1p1_200512-210012.nc` */
  def openDefault2(): nc2.NetcdfFile = openFile(defaultFile2)

  /** Opens a NetCDF file from a given path string. */
  def open(path: String): nc2.NetcdfFile = nc2.NetcdfFile.open(path).setImmutable()
  /** Opens a NetCDF file from a given file object . */
  def openFile(file: File): nc2.NetcdfFile = open(file.getPath)

  //  /** Convenient file object constructor from a given path string. */
  //  def file(path: String): File = new File(path)

  /** Boots the SuperCollider server. */
  def boot(): Unit =
    synth.Server.boot() {
      case _ => ()   // don't do anything specific now
    }

  private lazy val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'SysSon'", Locale.US)

  var showLog = false

  @elidable(CONFIG)  private[sysson] def logDebug(what: => String): Unit = log("debug", what)
  @elidable(INFO)    private[sysson] def logInfo (what: => String): Unit = log("info" , what)
  @elidable(WARNING) private[sysson] def logWarn (what: => String): Unit = log("warn" , what)

  private def log(level: String, what: => String): Unit =
    if (showLog) println(s"${logHeader.format(new Date())} - $level - $what")

  @elidable(CONFIG)  private[sysson] def logDebugTx(what: => String)(implicit tx: TxnLike): Unit =
    Txn.afterCommit(_ => logDebug(what))(tx.peer)

  @elidable(INFO)    private[sysson] def logInfoTx(what: => String)(implicit tx: TxnLike): Unit =
    Txn.afterCommit(_ => logInfo(what))(tx.peer)

  type Scale = Double => Double

  // ---- types ----

  private lazy val importExtensions: Unit = {
    Code.registerImports(Code.SynthGraph.id, Seq("at.iem.sysson.graph._"))
    Code.registerImports(LFScape.Code   .id, Seq("at.iem.sysson.fscape.graph._"))
  }

  def initTypes(): Unit = {
    // must come before Mellite.initTypes
    val ctlConf = Control.Config()
    ctlConf.terminateActors = false
    GenViewFactory.install(ctlConf)

    Mellite               .initTypes()
    matrix                .initTypes()
    Sonification          .init()
    Sonification.Source   .init()
    AuralSonification     .init()
    Plot                  .init()
    DoubleTransform       .init()
    importExtensions
  }
}

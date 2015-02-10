/*
 *  Report.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson.turbulence

import java.net.InetSocketAddress

import de.sciss.lucre.event.Sys
import de.sciss.osc

import scala.util.Try
import scala.util.control.NonFatal

object Report {
  final val DefaultRemoteHost = "172.17.0.101"
  final val DefaultRemotePort = 19821
  final val DefaultLocalHost  = "172.17.0.102"
  final val DefaultLocalPort  = 19822

  private val local: InetSocketAddress = {
    val host = sys.props.getOrElse("report-local-host", DefaultLocalHost)
    val port = sys.props.get("report-local-port").flatMap(n => Try(n.toInt).toOption).getOrElse(DefaultLocalPort)
    new InetSocketAddress(host, port)
  }

  private val target: InetSocketAddress = {
    val host = sys.props.getOrElse("report-remote-host", DefaultRemoteHost)
    val port = sys.props.get("report-remote-post").flatMap(n => Try(n.toInt).toOption).getOrElse(DefaultRemotePort)
    new InetSocketAddress(host, port)
  }

  private lazy val client: osc.UDP.Transmitter.Undirected = try {
    val config = osc.UDP.Config()
    config.localSocketAddress = local
    val res                   = osc.UDP.Transmitter(config)
    res.connect()
    res
  } catch {
    case NonFatal(e) =>
      Console.err.println("Error while creating 'report' transmitter:")
      e.printStackTrace()
      null
  }

  def dumpOSC(on: Boolean): Unit = {
    val c = client
    if (c != null) c.dump(if (on) osc.Dump.Text else osc.Dump.Off)
  }
  
  def init(): Unit = client

  private var failCount = 0
  
  def send[S <: Sys[S]](li: Int, varName: String, date: String)(implicit tx: S#Tx): Unit = tx.afterCommit {
    val m = osc.Message("/report", li, varName, date)
    try {
      val c = client
      if (c != null) c.send(m, target)
    } catch {
      case NonFatal(e) =>
        failCount += 1
        if (failCount < 100) Console.err.println(s"Report 'send' failed: ${e.getClass.getSimpleName}")
    }
  }
}
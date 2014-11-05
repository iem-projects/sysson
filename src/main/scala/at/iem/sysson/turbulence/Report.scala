package at.iem.sysson.turbulence

import java.net.{InetSocketAddress, InetAddress, SocketAddress}

import de.sciss.lucre.event.Sys
import de.sciss.osc

import scala.util.Try
import scala.util.control.NonFatal

object Report {
  final val DefaultTargetHost = "172.17.0.103"
  final val DefaultTargetPort = 19821

  private lazy val client = {
    val res = osc.UDP.Transmitter()
    try {
      res.connect()
    } catch {
      case NonFatal(e) =>
        Console.err.println("Error while connecting 'report' transmitter:")
        e.printStackTrace()
    }
    res
  }
  
  private val target: SocketAddress = {
    val host = sys.props.getOrElse("report-host", DefaultTargetHost)
    val port = sys.props.get("report-post").flatMap(n => Try(n.toInt).toOption).getOrElse(DefaultTargetPort)
    new InetSocketAddress(host, port)
  }
  
  def init(): Unit = client

  private var failCount = 0
  
  def send[S <: Sys[S]](li: Int, varName: String, date: String)(implicit tx: S#Tx): Unit = tx.afterCommit {
    val m = osc.Message("/report", li, varName, date)
    try {
      client.send(m, target)
    } catch {
      case NonFatal(e) =>
        failCount += 1
        if (failCount < 100) Console.err.println(s"Report 'send' failed: ${e.getClass.getSimpleName}")
    }
  }
}

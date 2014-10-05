/*
 *  OSCSupport.scala
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

package at.iem.sysson

import de.sciss.osc
import java.net.InetSocketAddress

object OSCSupport {
  import Implicits._
  import osc.Implicits._

  var target: InetSocketAddress = localhost -> 57120

  private lazy val trns = {
    val cfg   = osc.UDP.Config()
    cfg.codec = osc.PacketCodec().doubleToSinglePrecision().arrays()
    osc.UDP.Transmitter(cfg)
  }

  def send(name: String, args: Any*): Unit =
    trns.send(osc.Message(name, args: _*), target)

  implicit class OSCVarSect(val peer: VariableSection) extends AnyVal {
    def send(): Unit = {
      val data    = peer.readScaled1D()
      val dims    = peer.dimensions
      val ranges  = peer.ranges
      val info    = (dims zip ranges).zipWithIndex.flatMap { case ((dim, r), idx) =>
        Vector(dim.nameOption.getOrElse(idx), dim.size, r.start, r.last, r.step)
      }
      val args    = peer.name +: dims.size +: (info ++ (data.size +: data))
      val msg     = osc.Message("/sysson_matrix", args: _*)
      trns.send(msg, target)
    }
  }
}
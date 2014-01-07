/*
 *  OSCSupport.scala
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
      val infos   = (dims zip ranges).zipWithIndex.flatMap { case ((dim, r), idx) =>
        Vector(dim.nameOption.getOrElse(idx), dim.size, r.first, r.last, r.stride)
      }
      val args    = peer.name +: dims.size +: (infos ++ (data.size +: data))
      val msg     = osc.Message("/sysson_matrix", args: _*)
      trns.send(msg, target)
    }
  }
}
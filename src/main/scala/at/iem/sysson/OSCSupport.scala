package at.iem.sysson

import de.sciss.osc
import java.net.InetSocketAddress
import ucar.{nc2, ma2}

object OSCSupport {
  import Implicits._
  import osc.Implicits._

  var target: InetSocketAddress = localhost -> 57120

  private lazy val trns = {
    val cfg   = osc.UDP.Config()
    cfg.codec = osc.PacketCodec().doubleToSinglePrecision().arrays()
    osc.UDP.Transmitter(cfg)
  }

  def send(name: String, args: Any*) {
    trns.send(osc.Message(name, args: _*), target)
  }

  implicit class OSCVarSect(val peer: VariableSection) extends AnyVal {
    def send() {
      val data    = peer.readScaled1D()
      val dims    = peer.dimensions
      val ranges  = peer.ranges
      val infos   = (dims zip ranges).zipWithIndex.flatMap { case ((dim, r), idx) =>
        Vector(dim.name.getOrElse(idx), dim.size, r.first, r.last, r.stride)
      }
      val args    = peer.name +: dims.size +: (infos ++ (data.size +: data))
      val msg     = osc.Message("/sysson_matrix", args: _*)
      trns.send(msg, target)
    }
  }
}
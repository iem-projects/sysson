package at.iem.sysson

import de.sciss.osc
import java.net.InetSocketAddress
import ucar.{nc2, ma2}

object OSCSupport {
  import osc.Implicits._

  var target: InetSocketAddress = localhost -> 57120

  implicit class OSCVariable(val peer: nc2.Variable) extends AnyVal {
    def send() {
//      peer.read()
    }
  }
}
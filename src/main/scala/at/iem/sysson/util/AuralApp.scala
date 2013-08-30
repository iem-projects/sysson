package at.iem.sysson
package util

import de.sciss.synth.Server
import de.sciss.osc.TCP

trait AuralApp extends App {
  var s: Server = _

  override def main(args: Array[String]): Unit = {
    val c       = Server.Config()
    c.transport = TCP
    c.pickPort()
    val sync    = new AnyRef
    Server.run(c) { _s =>
      sync.synchronized {
        s = _s
        sync.notifyAll()
      }
    }
    sync.synchronized {
      sync.wait()
    }
    super.main(args)
  }
}
/*
 *  AuralApp.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

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
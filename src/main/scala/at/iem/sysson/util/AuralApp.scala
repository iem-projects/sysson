/*
 *  AuralApp.scala
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

package at.iem.sysson
package util

import de.sciss.synth.Server
import de.sciss.osc.TCP

trait AuralApp extends App {
  var s: Server = _

//  override def main(args: Array[String]): Unit = {
  private[this] val c       = Server.Config()
  c.transport = TCP
  c.pickPort()
  private[this] val sync    = new AnyRef
  Server.run(c) { _s =>
    sync.synchronized {
      s = _s
      sync.notifyAll()
    }
  }
  sync.synchronized {
    sync.wait()
  }
//    super.main(args)
//  }
}
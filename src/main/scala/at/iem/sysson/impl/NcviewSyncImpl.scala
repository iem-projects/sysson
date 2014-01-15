/*
 *  NcviewSyncImpl.scala
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
package impl

import de.sciss.osc
import gui.{GUI, DocumentViewHandler}
import de.sciss.model.impl.ModelImpl

private[sysson] object NcviewSyncImpl {
  def apply(config: NcviewSync.Config): NcviewSync = new Impl(config)

  private final class Impl(config: NcviewSync.Config) extends NcviewSync with ModelImpl[NcviewSync.Update] {
    private val sync      = new AnyRef
    private var channel   = Option.empty[osc.Channel]
    private var dumping   = false

    override def toString = "NcviewSync@" + hashCode().toHexString

    private def act(p: osc.Packet, sender: Any): Unit = p match {
      case osc.Bundle(time, ps @ _*) => ps.foreach(p1 => act(p1, sender))
      case osc.Message("/open", path: String) =>
//          dispatch(NcviewSync.Open(path))
//        DocumentHandler.instance.openRead(path)
        println("TODO: open document")

      //      case osc.Message("/var", path: String, name: String) => GUI.defer {
      //        for {
      //          doc  <- DocumentHandler.instance.getDocument(path)
      //          vr   <- doc.variableMap.get(name)
      //          view <- DocumentViewHandler.instance.getView(doc)
      //        } view.selectedVariable = Some(vr)
      //      }

      case _ => logWarn("Dropping unsupported OSC " + p)
    }

    def dump(on: Boolean) = sync.synchronized {
      channel.foreach(_.dump(if (on) osc.Dump.Text else osc.Dump.Off))
      dumping = on
      this
    }

    def start() = sync.synchronized {
      require(channel.isEmpty, "Already started")
      logInfo("Starting Ncview sonification server listening at " + config.protocol + " port " + config.port)
      val ch = config.protocol match {
        case osc.TCP =>
          val tcpCfg              = osc.TCP.Config()
          tcpCfg.localPort        = config.port
          tcpCfg.localIsLoopback  = config.loopback
          val srv                 = osc.Server(tcpCfg)
          srv.action              = act
          srv

        case osc.UDP =>
          val udpCfg              = osc.UDP.Config()
          udpCfg.localPort        = config.port
          udpCfg.localIsLoopback  = config.loopback
          val rcv                 = osc.UDP.Receiver(udpCfg)
          rcv.action              = act
          rcv
      }
      ch.connect()
      channel = Some(ch)
      if (dumping) ch.dump(osc.Dump.Text)
      this
    }

    def stop() = sync.synchronized {
      channel.foreach { ch =>
        ch.close()
        channel = None
      }
      this
    }

    def isRunning: Boolean = sync.synchronized(channel.isDefined)
  }
}
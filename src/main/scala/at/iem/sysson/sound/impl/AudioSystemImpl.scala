/*
 *  AudioSystemImpl.scala
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
package sound
package impl

import de.sciss.synth
import synth.Server
import de.sciss.model.impl.ModelImpl

object AudioSystemImpl {
  lazy val instance: AudioSystem = {
    val i = new Impl
    sys.addShutdownHook(i.stop())
    i
  }

  private final class Impl extends AudioSystem with ModelImpl[AudioSystem.Update] {
    override def toString = "AudioSystem"

    private val sync      = new AnyRef
    private var _server   = Option.empty[synth.ServerLike]

    def server: Option[synth.ServerLike] = sync.synchronized(_server)

    private def stopped(): Unit = sync.synchronized {
      _server = None
      dispatch(AudioSystem.Stopped)
    }

    private def started(server: synth.Server): Unit = sync.synchronized {
      _server = Some(server)
      dispatch(AudioSystem.Started(server))
      server.addListener {
        case synth.Server.Offline => stopped()
      }
    }

    def start(config: synth.Server.Config): this.type = sync.synchronized {
      require(_server.isEmpty, _server.get match {
        case _: synth.Server => "Server is already running"
        case _ => "Server is already booting"
      })
      val conn = synth.Server.boot(Main.name, config = config) {
        case synth.ServerConnection.Aborted => stopped()
        case synth.ServerConnection.Running(server) => started(server)
      }
      _server = Some(conn)
      this
    }

    def stop(): this.type = sync.synchronized {
      _server.foreach {
        case server: synth.Server => server.quit()
        case conn: synth.ServerConnection => conn.abort()
      }
      this
    }

    def isBooting: Boolean = sync.synchronized(_server match { case Some(_: synth.ServerConnection) => true; case _ => false })
    def isRunning: Boolean = sync.synchronized(_server match { case Some(_: synth.Server) => true; case _ => false })

    def whenBooted(fun: Server => Unit): this.type = {
      server match {
        case Some(s: Server) => fun(s)
        case _ =>
          lazy val list: AudioSystem.Listener = addListener {
            case AudioSystem.Started(s) =>
              removeListener(list)
              fun(s)
          }
          list
      }
      this
    }
  }
}
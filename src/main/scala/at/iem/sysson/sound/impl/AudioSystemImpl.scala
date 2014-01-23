/*
 *  AudioSystemImpl.scala
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
package sound
package impl

import de.sciss.{lucre, synth}
import de.sciss.model.impl.ModelImpl
import de.sciss.synth.proc.AuralSystem
import de.sciss.lucre.synth.Server

object AudioSystemImpl {
  lazy val instance: AudioSystem = {
    val aural = AuralSystem()
    val i = new Impl(aural)
    aural.addClient(i)
    // sys.addShutdownHook(i.stop())
    i
  }

  private final class Impl(val aural: AuralSystem)
    extends AudioSystem with ModelImpl[AudioSystem.Update] with AuralSystem.Client {

    override def toString = "AudioSystem"

    private val sync      = new AnyRef
    private var _server   = Option.empty[Server]

    def server: Option[Server] = sync.synchronized(_server)

    def stopped(): Unit = sync.synchronized {
      logInfo("SuperCollider Server stopped")
      _server = None
      dispatch(AudioSystem.Stopped)
    }

    def started(server: Server): Unit = sync.synchronized {
      logInfo("SuperCollider Server started")
      _server = Some(server)
      dispatch(AudioSystem.Started(server))
      //      server.addListener {
      //        case synth.Server.Offline => stopped()
      //      }
    }

    def start(config: synth.Server.Config): this.type = {
      aural.start(config)
      this
    }

    def stop(): this.type = {
      aural.stop()
      this
    }

    // def isBooting: Boolean = sync.synchronized(_server match { case Some(_: synth.ServerConnection) => true; case _ => false })
    // def isRunning: Boolean = sync.synchronized(_server match { case Some(_: synth.Server) => true; case _ => false })

    def isRunning: Boolean = sync.synchronized(_server.isDefined)

  //    def whenBooted(fun: Server => Unit): this.type = {
  //      aural.whenStarted(fun)
  //      this
  //    }

    def whenBooted(fun: Server => Unit): this.type = sync.synchronized {
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
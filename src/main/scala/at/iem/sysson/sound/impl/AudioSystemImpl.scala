/*
 *  AudioSystemImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package sound
package impl

import de.sciss.synth
import de.sciss.model.impl.ModelImpl
import de.sciss.synth.proc.AuralSystem
import de.sciss.lucre.synth.Server
import scala.concurrent.stm.{Ref, atomic}
import de.sciss.lucre.synth.{Txn => SynthTxn}

object AudioSystemImpl {
  lazy val instance: AudioSystem = {
    val aural = AuralSystem()
    val i = new Impl(aural)
    atomic { implicit tx =>
      implicit val ptx = SynthTxn.wrap(tx)
      aural.addClient(i)
    }
    // sys.addShutdownHook(i.stop())
    i
  }

  private final class Impl(val aural: AuralSystem)
    extends AudioSystem with ModelImpl[AudioSystem.Update] with AuralSystem.Client {

    override def toString = "AudioSystem"

    private val _server   = Ref(Option.empty[Server])

    def server: Option[Server] = _server.single()

    def stopped()(implicit tx: SynthTxn): Unit = {
      logInfo("SuperCollider Server stopped")
      _server.set(None)(tx.peer)
      tx.afterCommit(dispatch(AudioSystem.Stopped))
    }

    def started(server: Server)(implicit tx: SynthTxn): Unit = {
      logInfo("SuperCollider Server started")
      _server.set(Some(server))(tx.peer)
      tx.afterCommit(dispatch(AudioSystem.Started(server)))
    }

    def start(config: synth.Server.Config): this.type = {
      atomic { implicit tx =>
        implicit val ptx = SynthTxn.wrap(tx)
        aural.start(config)
      }
      this
    }

    def stop(): this.type = {
      atomic { implicit tx =>
        implicit val ptx = SynthTxn.wrap(tx)
        aural.stop()
      }
      this
    }

    // def isBooting: Boolean = sync.synchronized(_server match { case Some(_: synth.ServerConnection) => true; case _ => false })
    // def isRunning: Boolean = sync.synchronized(_server match { case Some(_: synth.Server) => true; case _ => false })

    def isRunning: Boolean = _server.single().isDefined

  //    def whenBooted(fun: Server => Unit): this.type = {
  //      aural.whenStarted(fun)
  //      this
  //    }

    // XXX TODO: not 100% safe - should just forward to AuralSystem
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
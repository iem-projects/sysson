package at.iem.sysson
package sound
package impl

import at.iem.sysson.impl.ModelImpl
import de.sciss.synth

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

    private def stopped() {
      sync.synchronized {
        _server = None
        dispatch(AudioSystem.Stopped)
      }
    }

    private def started(server: synth.Server) {
      sync.synchronized {
        _server = Some(server)
        dispatch(AudioSystem.Started(server))
        server.addListener {
          case synth.Server.Offline => stopped()
        }
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
  }
}
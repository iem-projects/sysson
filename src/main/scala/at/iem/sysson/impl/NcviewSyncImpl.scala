package at.iem.sysson
package impl

import de.sciss.osc
import gui.{GUI, DocumentViewHandler}

private[sysson] object NcviewSyncImpl {
  def apply(config: NcviewSync.Config): NcviewSync = new Impl(config)

  private final class Impl(config: NcviewSync.Config) extends NcviewSync with ModelImpl[NcviewSync.Update] {
    private val sync      = new AnyRef
    private var channel   = Option.empty[osc.Channel]
    private var dumping   = false

    override def toString = "NcviewSync@" + hashCode().toHexString

    private def act(p: osc.Packet, sender: Any) {
      p match {
        case osc.Bundle(time, ps @ _*) => ps.foreach(p1 => act(p1, sender))
        case osc.Message("/open", path: String) =>
//          dispatch(NcviewSync.Open(path))
          DocumentHandler.instance.openRead(path)

        case osc.Message("/var", path: String, name: String) => GUI.defer {
          for {
            doc  <- DocumentHandler.instance.getDocument(path)
            vr   <- doc.variableMap.get(name)
            view <- DocumentViewHandler.instance.getView(doc)
          } view.selectedVariable = Some(vr)
        }

        case _ => logWarn("Dropping unsupported OSC " + p)
      }
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
/*
 *  MainViewImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui
package impl

import javax.swing.{BorderFactory, ImageIcon}
import de.sciss.mellite.{Mellite, Prefs}
import de.sciss.synth.proc.AuralSystem

import scala.concurrent.stm.TxnExecutor
import swing.{Swing, Alignment, Label, Orientation, BoxPanel}
import de.sciss.synth.swing.ServerStatusPanel
import de.sciss.audiowidgets.PeakMeter
import java.awt.Color
import Swing._
import de.sciss.desktop.impl.DynamicComponentImpl
import de.sciss.lucre.synth.{Txn, Server}
import de.sciss.osc
import de.sciss.lucre.swing._

private[gui] object MainViewImpl {
  def apply(background: Option[Color] = None): MainView = {
    requireEDT()
    val res = new Impl(background)
    if (AUTO_BOOT) res.boot()
    res
  }

  private def AUTO_BOOT = Prefs.audioAutoBoot.getOrElse(false)

  private lazy val logo = new ImageIcon(Main.getClass.getResource("SysSon-Logo_web_noshadow.png"))

  private final class Impl(bg: Option[Color]) extends MainView {
    def boot(): Unit = Mellite.startAuralSystem() // atomic { implicit tx => Mellite.auralSystem.start() }

    private lazy val serverStatus = new ServerStatusPanel {
      bootAction = Some(boot _)
    }

    //      background = Color.black

    private def deferAudioUpdated(sOpt: Option[Server]): Unit = defer {
      sOpt match {
        case Some(server /* : synth.Server */) =>
          serverStatus.server   = Some(server.peer)
          startMeters(server)
        // case Some(conn: synth.ServerConnection) =>
        //   serverStatus.booting  = Some(conn)
        case _ =>
          serverStatus.server   = None
      }
    }

    private def startMeters(server: Server): Unit = {
      import de.sciss.synth._
      import ugen._

      val numChannels = 2 // fixed for now...
      val offset      = 0

      val d = SynthDef("$sysson_master_meter" + numChannels) {
        val sig   = In.ar("bus".ir, numChannels)
        val tr    = Impulse.kr(20)
        val peak  = Peak.kr(sig, tr)
        val rms   = A2K.kr(Lag.ar(sig.squared, 0.1))
        SendReply.kr(tr, Flatten(Zip(peak, rms)), "/$meter")
      }

      val syn     = Synth(server.peer, id = 0x10000000) // XXX TODO: id dirty...
      val newMsg  = syn.newMsg(d.name, target = server.peer.defaultGroup, args = Seq("bus" -> offset), addAction = addAfter)

      val resp    = message.Responder.add(server.peer) {
        case osc.Message("/$meter", syn.id, _, vals @ _*) =>
          val pairs = vals.asInstanceOf[Seq[Float]].toIndexedSeq
          val time  = System.currentTimeMillis()
          Swing.onEDT(mainMeters.update(pairs, 0, time))
      }

      val recvMsg = d.recvMsg(completion = newMsg)

      syn.onEnd { resp.remove() }

      server ! recvMsg
    }

    private lazy val audioListener: AuralSystem.Client = new AuralSystem.Client {
      def auralStarted(s: Server)(implicit tx: Txn): Unit = tx.afterCommit(deferAudioUpdated(Some(s)))
      def auralStopped()         (implicit tx: Txn): Unit = tx.afterCommit(deferAudioUpdated(None   ))
    }

    private def atomic[A](fun: Txn => A): A = TxnExecutor.defaultAtomic { itx =>
      val tx = Txn.wrap(itx)
      fun(tx)
    }

    private lazy val mainMeters = new PeakMeter {
      numChannels   = 2
      orientation   = Orientation.Horizontal
//      borderVisible = true
//      hasCaption    = true  // only works properly for vertically oriented meters at the moment
    }

    private lazy val box1 = new BoxPanel(Orientation.Horizontal) {
      bg.foreach(background = _)
      contents += new Label(null, logo, Alignment.Center) {
        border = BorderFactory.createEmptyBorder(4, 4, 4, 4)
      }
    }

    lazy val component = new BoxPanel(Orientation.Vertical) with DynamicComponentImpl {
      bg.foreach(background = _)
      contents += box1
      contents += mainMeters
      contents += RigidBox((2, 4))
      contents += serverStatus

      protected def componentShown(): Unit = {
        val sOpt = atomic { implicit tx =>
          val as = Mellite.auralSystem
          as.addClient(audioListener)
          as.serverOption
        }
        deferAudioUpdated(sOpt)
      }

      protected def componentHidden(): Unit =
        atomic { implicit tx =>
          Mellite.auralSystem.removeClient(audioListener)
        }
    }
  }
}
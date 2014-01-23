/*
 *  MainViewImpl.scala
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
package gui
package impl

import javax.swing.{BorderFactory, ImageIcon}
import swing.{Swing, Alignment, Label, Orientation, BoxPanel}
import de.sciss.synth.swing.ServerStatusPanel
import de.sciss.audiowidgets.PeakMeter
import java.awt.Color
import Swing._
import sound.AudioSystem
import GUI.{defer, requireEDT}
import de.sciss.{osc, synth}
import de.sciss.desktop.impl.DynamicComponentImpl
import de.sciss.lucre.synth.Server

private[gui] object MainViewImpl {
  def apply(background: Option[Color] = None): MainView = {
    requireEDT()
    new Impl(background)
  }

  private lazy val logo = new ImageIcon(Main.getClass.getResource("SysSon-Logo_web_noshadow.png"))

  private final class Impl(bg: Option[Color]) extends MainView with DynamicComponentImpl {
    private def boot(): Unit = AudioSystem.instance.start()

    private lazy val serverStatus = new ServerStatusPanel {
      bootAction = Some(boot _)
    }

    //      background = Color.black

    private def deferAudioUpdated(): Unit = defer {
      val s = AudioSystem.instance.server
      s match {
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
//      import synth._
//      import ugen._
//
//      val numChannels = 2 // fixed for now...
//      val offset      = 0
//
//      val d = SynthDef("$sysson_master_meter" + numChannels) {
//        val sig   = In.ar("bus".ir, numChannels)
//        val tr    = Impulse.kr(20)
//        val peak  = Peak.kr(sig, tr)
//        val rms   = A2K.kr(Lag.ar(sig.squared, 0.1))
//        SendReply.kr(tr, Flatten(Zip(peak, rms)), "/$meter")
//      }
//
//      val syn     = Synth(server)
//      val newMsg  = syn.newMsg(d.name, target = server.defaultGroup, args = Seq("bus" -> offset), addAction = addAfter)
//
//      val resp    = message.Responder.add(server) {
//        case osc.Message("/$meter", syn.id, _, vals @ _*) =>
//          val pairs = vals.asInstanceOf[Seq[Float]].toIndexedSeq
//          val time  = System.currentTimeMillis()
//          Swing.onEDT(mainMeters.update(pairs, 0, time))
//      }
//
//      val recvMsg = d.recvMsg(completion = newMsg)
//
//      syn.onEnd { resp.remove() }
//
//      server ! recvMsg
    }

    private lazy val audioListener: AudioSystem.Listener = {
      // case AudioSystem.Booting(_) => deferAudioUpdated()
      case AudioSystem.Started(_) => deferAudioUpdated()
      case AudioSystem.Stopped    => deferAudioUpdated()
    }

    protected def componentShown(): Unit = {
      AudioSystem.instance.addListener(audioListener)
      deferAudioUpdated()
    }


    protected def componentHidden(): Unit =
      AudioSystem.instance.removeListener(audioListener)

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

    lazy val component = new BoxPanel(Orientation.Vertical) {
      bg.foreach(background = _)
      contents += box1
      contents += mainMeters
      contents += RigidBox((2, 4))
      contents += serverStatus
    }
  }
}
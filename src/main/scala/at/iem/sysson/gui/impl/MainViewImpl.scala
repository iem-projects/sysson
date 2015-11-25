/*
 *  MainViewImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 Hanns Holger Rutz. All rights reserved.
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

import java.awt.geom.AffineTransform
import java.awt.{RenderingHints, Color, Font, Graphics}
import javax.imageio.ImageIO
import javax.swing.{BorderFactory, Icon, ImageIcon}

import de.sciss.audiowidgets.PeakMeter
import de.sciss.desktop.Desktop
import de.sciss.desktop.impl.DynamicComponentImpl
import de.sciss.lucre.swing._
import de.sciss.lucre.synth.{Server, Txn}
import de.sciss.mellite.{Mellite, Prefs}
import de.sciss.osc
import de.sciss.synth.proc.AuralSystem
import de.sciss.synth.swing.ServerStatusPanel

import scala.concurrent.stm.TxnExecutor
import scala.swing.Swing._
import scala.swing.{Graphics2D, Alignment, BoxPanel, Label, Orientation, Swing}

private[gui] object MainViewImpl {
  def apply(background: Option[Color] = None): MainView = {
    requireEDT()
    val res = new Impl(background)
    if (AUTO_BOOT) res.boot()
    res
  }

  private def AUTO_BOOT = Prefs.audioAutoBoot.getOrElse(false)

  private lazy val logo: Icon = {
    val is = Main.getClass.getResourceAsStream("SysSon-Logo_noshadow_566px.png")
    if (is == null) new Icon {
      val getIconHeight = 109
      val getIconWidth  = 283
      def paintIcon(c: java.awt.Component, g: Graphics, x: Int, y: Int): Unit = {
        g.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 64))
        g.setColor(Color.white)
        g.drawString("SysSon", 32, 80)
      }
    } else try {
      val img = ImageIO.read(is)
      new ImageIcon(img) {
        override def getIconWidth : Int = 283 // 566
        override def getIconHeight: Int = 109 // 218

        private val at = new AffineTransform

        override def paintIcon(c: java.awt.Component, g: Graphics, x: Int, y: Int): Unit = synchronized {
          val imageObserver = getImageObserver
          val obs           = if (imageObserver == null) c else imageObserver
          val image         = getImage
          val g2            = g.asInstanceOf[Graphics2D]
          g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
          g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING , RenderingHints.VALUE_ANTIALIAS_ON         )
          at.setToTranslation(x, y)
          at.scale(0.5, 0.5)
          g2.drawImage(image, at, obs)
        }
      }
    } finally {
      is.close()
    }
  }

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
          Swing.onEDT {
            mainMeters.update(pairs, 0, time)
            if (Desktop.isLinux) mainMeters.toolkit.sync()  // cf. https://github.com/Sciss/AudioWidgets/issues/6
          }
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
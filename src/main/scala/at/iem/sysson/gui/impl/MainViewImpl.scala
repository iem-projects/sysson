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
import de.sciss.synth

private[gui] object MainViewImpl {
  def apply(): MainView = {
    requireEDT()
    new Impl
  }

  private lazy val logo = new ImageIcon(Main.getClass.getResource("SysSon-Logo_web_noshadow.png"))

  private final class Impl extends MainView with DynamicView {
    private def boot() {
      AudioSystem.instance.start()
    }

    private lazy val serverStatus = new ServerStatusPanel {
      bootAction = Some(boot _)
    }
//    {
//      background = Color.black
//    }

    private def deferAudioUpdated() {
      defer {
        val s = AudioSystem.instance.server
        s match {
          case Some(server: synth.Server) =>
            serverStatus.server   = Some(server)
          case Some(conn: synth.ServerConnection) =>
            serverStatus.booting  = Some(conn)
          case _ =>
            serverStatus.server   = None
        }
      }
    }

    private lazy val audioListener: AudioSystem.Listener = {
      case AudioSystem.Booting(_) => deferAudioUpdated()
      case AudioSystem.Started(_) => deferAudioUpdated()
      case AudioSystem.Stopped    => deferAudioUpdated()
    }

    protected def componentOpened() {
      AudioSystem.instance.addListener(audioListener)
      deferAudioUpdated()
    }


    protected def componentClosed() {
      AudioSystem.instance.removeListener(audioListener)
    }

    private lazy val mainMeters  = new PeakMeter {
      numChannels   = 2
      orientation   = Orientation.Horizontal
//      borderVisible = true
//      hasCaption    = true  // only works properly for vertically oriented meters at the moment
    }

    private lazy val box1 = new BoxPanel(Orientation.Horizontal) {
      background = Color.white // .black
      contents += new Label(null, logo, Alignment.Center) {
        border = BorderFactory.createEmptyBorder(4, 4, 4, 4)
      }
    }

    lazy val component = new BoxPanel(Orientation.Vertical) {
      background = Color.white // black
      contents += box1
      contents += mainMeters
      contents += RigidBox((2, 4))
      contents += serverStatus
    }
  }
}
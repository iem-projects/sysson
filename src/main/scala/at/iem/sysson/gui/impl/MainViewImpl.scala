package at.iem.sysson
package gui
package impl

import javax.swing.{BorderFactory, ImageIcon}
import swing.{Swing, Alignment, Label, Orientation, BoxPanel}
import de.sciss.synth.swing.ServerStatusPanel
import de.sciss.audiowidgets.PeakMeter
import java.awt.Color
import Swing._

private[gui] object MainViewImpl {
  def apply(): MainView = {
    GUI.requireEDT()
    new Impl
  }

  private lazy val logo = new ImageIcon(Main.getClass.getResource("SysSon-Logo_web_noshadow.png"))

  private final class Impl extends MainView {
    private def boot() {
      println("BOOT")
    }

    private val serverStatus = new ServerStatusPanel {
      bootAction = Some(boot _)
    }
//    {
//      background = Color.black
//    }

    private val mainMeters  = new PeakMeter {
      numChannels   = 2
      orientation   = Orientation.Horizontal
//      borderVisible = true
//      hasCaption    = true  // only works properly for vertically oriented meters at the moment
    }

    private val box1 = new BoxPanel(Orientation.Horizontal) {
      background = Color.black
      contents += new Label(null, logo, Alignment.Center) {
        border = BorderFactory.createEmptyBorder(4, 4, 4, 4)
      }
    }

    val component = new BoxPanel(Orientation.Vertical) {
      background = Color.black
      contents += box1
      contents += mainMeters
      contents += RigidBox((2, 4))
      contents += serverStatus
    }
  }
}
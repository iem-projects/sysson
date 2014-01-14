package at.iem.sysson.gui

import de.sciss.swingplus.OverlayPanel
import scala.swing.{Swing, ProgressBar}
import Swing._

class SpinningProgressBar extends OverlayPanel {
  @volatile private var _spin = false

  /** This method is thread safe. */
  def spinning: Boolean = _spin
  def spinning_=(value: Boolean): Unit = {
    _spin = value
    GUI.defer {
      ggBusy.visible = _spin
    }
  }

  private val ggBusy = new ProgressBar {
    visible       = false
    indeterminate = true
    preferredSize = (24, 24)
    peer.putClientProperty("JProgressBar.style", "circular")
  }

  contents += RigidBox((24, 24))
  contents += ggBusy
}

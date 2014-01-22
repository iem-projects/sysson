/*
 *  SpinningProgressBar.scala
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

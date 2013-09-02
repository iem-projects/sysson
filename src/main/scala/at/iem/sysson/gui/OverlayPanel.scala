package at.iem.sysson.gui

import scala.swing.{SequentialContainer, Panel}
import javax.swing.{OverlayLayout, JPanel}

class OverlayPanel extends Panel with SequentialContainer.Wrapper {
  override lazy val peer: JPanel = {
    val res = new JPanel(null) with SuperMixin
    res.setLayout(new OverlayLayout(res))
    res
  }
}

package at.iem.sysson.sound.designer.impl

import java.awt.{Font, GraphicsEnvironment}
import prefuse.util.ColorLib

private[impl] object Style {
  final val font: Font = {
    val fntNames  = GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames
    val fntMenlo  = "Menlo"
    val name      = if (fntNames.contains(fntMenlo)) {
      fntMenlo
    } else {
      Font.MONOSPACED
    }
    new Font(name, Font.PLAIN, 11)
  }

  final val selectionColor  = ColorLib.getColor(  0,   0, 240)
  final val boxColor        = ColorLib.getColor(246, 248, 248)
}
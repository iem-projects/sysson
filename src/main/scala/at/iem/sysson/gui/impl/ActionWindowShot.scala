package at.iem.sysson.gui
package impl

import scala.swing.{Dimension, Graphics2D, Action}
import scala.swing.event.Key
import de.sciss.{desktop, pdflitz}

class ActionWindowShot(w: desktop.Window) extends Action("Window Screen-Shot") {

  import de.sciss.desktop.KeyStrokes
  import KeyStrokes._

  accelerator = Some(menu1 + shift + Key.P)

  def apply(): Unit = windowShot()

  private def windowShot(): Unit = {
    val c = new pdflitz.Generate.Source {
      import w.component
      def render(g: Graphics2D): Unit = component.peer.paint(g)

      def preferredSize: Dimension = component.preferredSize

      def size: Dimension = component.size
    }
    new pdflitz.SaveAction(c :: Nil).apply()
  }
}
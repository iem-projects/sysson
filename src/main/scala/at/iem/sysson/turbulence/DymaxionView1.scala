package at.iem.sysson.turbulence

import java.awt.Color
import java.awt.geom.Ellipse2D
import javax.swing.ImageIcon

import at.iem.sysson.turbulence.Dymaxion._

import scala.swing.{Swing, Graphics2D, Component}
import Swing._

class DymaxionView1 extends Component {
  private val url         = getClass.getResource("fmap1.gif")
  private val image       = new ImageIcon(url)

  private var _mark = Option.empty[Pt2]

  private val circle      = new Ellipse2D.Float()
  private val gainRadius  = 16

  private val w = 615
  private val h = 326

  private val hSz = 106
  private val vSz = 106

  private val hOff = 58
  private val vOff = 29

  preferredSize = (w, h)

  def mark: Option[Pt2] = _mark
  def mark_=(value: Option[Pt2]): Unit = if (_mark != value) {
    _mark = value
    repaint()
  }

  override protected def paintComponent(g: Graphics2D): Unit = {
    image.paintIcon(peer, g, 0, 0)

    mark.foreach { case (vx, vy) =>
      val xp  = (vx - 0.5) * hSz + hOff
      val yp  = h - (vy + 1.0/math.sqrt(3)) * vSz + vOff
      circle.setFrameFromCenter(xp, yp, xp + 12, yp + 12)
      g.setColor(Color.yellow)
      g.fill(circle)
    }
  }
}

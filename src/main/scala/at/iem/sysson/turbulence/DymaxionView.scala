package at.iem.sysson
package turbulence

import java.awt.geom.{Path2D, GeneralPath}
import java.awt.{Cursor, Color, Toolkit}
import javax.swing.ImageIcon

import scala.swing.event.MouseMoved
import scala.swing.{Swing, Graphics2D, Component}
import Swing._

class DymaxionView extends Component {
  // private val image = Toolkit.getDefaultToolkit.createImage(getClass.getClassLoader.getResource("dymaxion.png"))
  private val url         = getClass.getResource("dymaxion.png")
  private val image       = new ImageIcon(url)

  private val hNum        = 13
  private val hSz         = 92
  private val vNum        = 9 // 3
  private val vSz         = 53.33333f // 160
  private val vSz1        = vSz * 2

  private val gainRadius  = 16
  private val w           = hNum * hSz  // 1196
  private val w1          = w + gainRadius + gainRadius
  private val h           = (vNum * vSz + 0.5f).toInt //  480
  private val h1          = h + gainRadius + gainRadius

  // private var tri = ((-1, -1), (-1, -1), (-1, -1))

  private val gp          = new GeneralPath(Path2D.WIND_NON_ZERO, 4)
  private val colrTri     = new Color(0xFF, 0x00, 0x00, 0x7F)

  override protected def paintComponent(g: Graphics2D): Unit = {
    g.setColor(Color.gray)
    g.fillRect(0, 0, w1, h1)
    // g.drawImage(image, 0, 0, peer)
    image.paintIcon(peer, g, gainRadius, gainRadius)

    g.setColor(colrTri)
    g.fill(gp)
  }

  preferredSize = (w1, h1)
  listenTo(mouse.moves)
  cursor = Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR)

  reactions += {
    case MouseMoved(_, pt, _) =>
      // cf. https://stackoverflow.com/questions/8043947/indexing-in-equilateral-triangle-grid-given-simple-2d-cartesian-coordinates
      val xf  = (pt.x.toFloat - gainRadius) / hSz
      val x   = xf.toInt
      val u   = xf - x
      val yf  = (pt.y.toFloat - gainRadius) / vSz
      val y0  = yf.toInt
      val v   = yf - y0

      // val x   = if (v - u > 0 || ((((x0 % 2) ^ (y % 2)) == 1) || v + u < 1)) x0 - 1 else x0
      val y   = if (u - v > 0 || ((((y0 % 2) ^ (x % 2)) == 1) || u + v < 1)) y0 - 1 else y0

      val vx1 = x + y%2
      val vy1 = y/2 + y%2

      val vx2 = x + (y%2^x%2)
      val vy2 = y/2 + (1-(y%2))

      val vx3 = x + (1-(y%2))
      val vy3 = y/2 + (y%2)

      println(s"x = $x, y = $y")
      println(s"triangle = [$vx1,$vy1; $vx2,$vy2; $vx3,$vy3]")

      def move(x: Int, y: Int, first: Boolean): Unit = {
        val y1  = 2 * y + x % 2
        val xp  = x  * hSz + gainRadius
        val yp  = y1 * vSz + gainRadius
        if (first) gp.moveTo(xp, yp) else gp.lineTo(xp, yp)
      }

      gp.reset()
      move(vx1, vy1, first = true )
      move(vx2, vy2, first = false)
      move(vx3, vy3, first = false)
      // repaint(gp.getBounds)
      repaint()
  }
}

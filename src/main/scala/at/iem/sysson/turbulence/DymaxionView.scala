package at.iem.sysson
package turbulence

import java.awt.geom.{Ellipse2D, Path2D, GeneralPath}
import java.awt.{BasicStroke, Cursor, Color}
import javax.swing.ImageIcon

import scala.swing.event.MouseMoved
import scala.swing.{Swing, Graphics2D, Component}
import Swing._

class DymaxionView extends Component {
  private val DRAW_SPKR       = true
  private val DRAW_SPKR_IDX   = true

  // private val image = Toolkit.getDefaultToolkit.createImage(getClass.getClassLoader.getResource("dymaxion.png"))
  private val url         = getClass.getResource("dymaxion.png")
  private val image       = new ImageIcon(url)

  private val hNum        = 13
  private val hSz         = 92
  private val vNum        = 9 // 3
  private val vSz         = 53.33333f // 160
  private val vNum1       = vNum / 2

  private val numCols     = hNum  + 1
  private val numRows     = vNum1 + 1

  private val gainRadius  = 16
  private val w           = hNum * hSz  // 1196
  private val w1          = w + gainRadius + gainRadius
  private val h           = (vNum * vSz + 0.5f).toInt //  480
  private val h1          = h + gainRadius + gainRadius

  // private var tri = ((-1, -1), (-1, -1), (-1, -1))

  private val gpFill      = new GeneralPath(Path2D.WIND_NON_ZERO,  4)
  private val gpStroke    = new GeneralPath(Path2D.WIND_NON_ZERO, 20)
  private val circle      = new Ellipse2D.Float()
  private val colrTri     = new Color(0xFF, 0x00, 0x00, 0x7F)
  private val colrSpkr    = new Color(0x00, 0xFF, 0xFF, 0x7F)
  private val colrEmpty   = new Color(0x00, 0x00, 0x00, 0x4F)
  private val colrGain    = Color.blue
  // private val strkGain    = new BasicStroke(2f, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10f, Array(2f, 2f), 0f)
  private val strkGain    = new BasicStroke(2f)

  override protected def paintComponent(g: Graphics2D): Unit = {
    g.setColor(Color.gray)
    g.fillRect(0, 0, w1, h1)
    // g.drawImage(image, 0, 0, peer)
    image.paintIcon(peer, g, gainRadius, gainRadius)

    if (DRAW_SPKR) {
      var vx = 0; while (vx < numCols) {
        var vyi = 0; while (vyi < numRows) {
          val vy  = vyi * 2 + (vx % 2)
          val xp  = vx * hSz + gainRadius
          val yp  = vy * vSz + gainRadius
          circle.setFrameFromCenter(xp, yp, xp + 12, yp + 12)
          val chanOpt = Turbulence.ChannelMap.get((vx, vyi))
          g.setColor(if (chanOpt.isDefined) colrSpkr else colrEmpty)
          g.fill(circle)
          if (DRAW_SPKR_IDX) {
            g.setColor(Color.black)
            g.drawString(s"$vx,$vyi", xp + 10, yp - 5)
            g.setColor(Color.white)
            g.drawString(s"$vx,$vyi", xp +  9, yp - 6)
            chanOpt.foreach { ch =>
              g.setColor(Color.blue)
              val s = ch.toString
              g.drawString(ch.toString, xp + (if (s.length == 1) -4 else -7), yp + 4)
            }
          }
          vyi += 1
        }
        vx += 1
      }
    }

    g.setColor(colrTri)
    g.fill(gpFill)
    val strkOrig = g.getStroke
    g.setColor(colrGain)
    g.setStroke(strkGain)
    g.draw(gpStroke)
    g.setStroke(strkOrig)
  }

  preferredSize = (w1, h1)
  listenTo(mouse.moves)
  cursor = Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR)

  reactions += {
    case MouseMoved(_, pt, _) =>
      // cf. https://stackoverflow.com/questions/8043947/indexing-in-equilateral-triangle-grid-given-simple-2d-cartesian-coordinates
      val xf  = math.max(0f, (pt.x.toFloat - gainRadius) / hSz)
      val x   = xf.toInt
      val u   = xf - x
      val yf  = math.max(0f, (pt.y.toFloat - gainRadius) / vSz)
      val y0  = yf.toInt
      val v   = yf - y0

      // note, we add two to avoid problems with negative numbers,
      // but have to take care to subtract that later
      val y   =
        if (((y0 % 2) ^ (x % 2)) == 1) {
          if (u + v < 1) y0 + 1 else y0 + 2
        } else {
          if (u - v > 0) y0 + 1 else y0 + 2
        }

      val yDiv  = y / 2 - 1 // the minus 1 corrects the offset
      val yOdd  = y % 2
      val yEven = 1 - yOdd
      val xOdd  = x % 2

      val vx1  = x + yOdd
      val vy1i = yDiv + yOdd
      val vy1  = vy1i * 2 + (vx1 % 2)

      val vx2  = x + (yOdd ^ xOdd)
      val vy2i = yDiv + yEven
      val vy2  = vy2i * 2 + (vx2 % 2)

      val vx3  = x + yEven
      val vy3i = yDiv + yOdd
      val vy3  = vy3i * 2 + (vx3 % 2)

      //      println(s"x = $x, y = $y")
      // println(s"triangle = [$vx1,$vy1i; $vx2,$vy2i; $vx3,$vy3i]")

      // cf. https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
      def dist(x: Float, y: Float)(lx1: Int, ly1: Int, lx2: Int, ly2: Int): Float = {
        // |Dy x0 - Dx y0 - x1 y2 + x2 y1| / sqrt(Dx.squared + Dy.squared)
        // - we assume a normalized Dx, Dy, and thus drop the division
        // - indeed we know the height is two and divide by it

        val dx = lx2 - lx1
        val dy = ly2 - ly1
        math.abs(dy * x - dx * y - lx1 * ly2 + lx2 * ly1) / 2
      }

      val df = dist(xf, yf) _
      val d1 = df(vx2, vy2, vx3, vy3)
      val d2 = df(vx3, vy3, vx1, vy1)
      val d3 = df(vx1, vy1, vx2, vy2)
      // println(f"d1 = $d1%1.2f, d2 = $d2%1.2f, d3 = $d3%1.2f, sum = ${d1 + d2 + d3}%1.2f")
      val g1 = math.sqrt(d1)
      val g2 = math.sqrt(d2)
      val g3 = math.sqrt(d3)
      // println(f"gain1 = $g1%1.2f, gain2 = $g2%1.2f, gain3 = $g3%1.2f, power-sum = ${math.sqrt(g1*g1 + g2*g2 + g3*g3)}%1.2f")

      def move(x: Int, y: Int, gain: Double, first: Boolean): Unit = {
        val xp  = x * hSz + gainRadius
        val yp  = y * vSz + gainRadius
        if (first) gpFill.moveTo(xp, yp) else gpFill.lineTo(xp, yp)
        val g = gain * 12
        circle.setFrameFromCenter(xp, yp, xp + g, yp + g)
        gpStroke.append(circle, false)
      }

      gpFill  .reset()
      gpStroke.reset()
      move(vx1, vy1, g1, first = true )
      move(vx2, vy2, g2, first = false)
      move(vx3, vy3, g3, first = false)
      // repaint(gp.getBounds)
      repaint()
  }
}

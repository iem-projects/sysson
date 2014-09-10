package at.iem.sysson
package turbulence

import java.awt.{Color, Cursor}
import java.awt.image.BufferedImage
import javax.swing.ImageIcon

import de.sciss.numbers

import scala.swing.event.{MouseDragged, MousePressed, MouseMoved}
import scala.swing.{Point, Rectangle, Swing, Graphics2D, Component}
import numbers.Implicits._
import Swing._

class MercatorView(dymaxion: DymaxionView) extends Component {
  private val url         = getClass.getResource("mercator.jpg")
  private val image       = new ImageIcon(url)

  private val w = 800
  private val h = 400

  private val test  = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
  private val testG = test.createGraphics()

  preferredSize = (w, h)

  for (x <- 0 until w by 2)
    for (y <- 0 until h by 2)
      mkPt(x, y)

  override protected def paintComponent(g: Graphics2D): Unit = {
    image.paintIcon(peer, g, 0, 0)
    g.drawImage(test, 0, 0, peer)
  }

  cursor = Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR)

  listenTo(mouse.moves)
  listenTo(mouse.clicks)

  reactions += {
    case MousePressed(_, pt, _, _, _) => process(pt)
    case MouseDragged(_, pt, _) => process(pt)
  }

  private def process(pt: Point): Unit = {
    val lon = pt.x.linlin(0, w, -180, 180)
    val lat = pt.y.linlin(0, h,   90, -90)
    tooltip = f"lat = $lat%1.2f, lon = $lon%1.2f"
    val idx = Dymaxion.findFaceIndex   (lon = lon, lat = lat)
    val (h0x, h0y) = Dymaxion.mapLonLat(lon = lon, lat = lat)
    // val (h0x, h0y) = Dymaxion.mapCartesian(Dymaxion.center(idx))
    // mkPt(pt.x, pt.y)
    // repaint(new Rectangle(pt.x, pt.y, 1, 1))
    println(f"idx = $idx, x = $h0x%1.2f, y = $h0y%1.2f")

    dymaxion.mark = Some((h0x, h0y))
  }

  def mkPt(x: Int, y: Int): Unit = {
    val lon = x.linlin(0, w, -180, 180)
    val lat = y.linlin(0, h,   90, -90)
    val idx = Dymaxion.findFaceIndex(lon = lon, lat = lat)
    val colr = Color.getHSBColor(idx.linlin(0, 19, 0.0f, 1.0f), 1f, 1f)
    testG.setColor(colr)
    testG.fillRect(x, y, 1, 1)
  }
}

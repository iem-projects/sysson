/*
 *  MercatorView.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package turbulence

import java.awt.{Color, Cursor}
import java.awt.image.BufferedImage
import javax.swing.ImageIcon

import at.iem.sysson.turbulence.Turbulence.{Radians, LatLon}
import de.sciss.numbers

import scala.swing.event.{MouseReleased, MouseDragged, MousePressed}
import scala.swing.{Point, Swing, Graphics2D, Component}
import numbers.Implicits._
import Swing._

class MercatorView(dymaxion: DymaxionView) extends Component {
  private val url         = getClass.getResource("mercator.jpg")
  private val image       = new ImageIcon(url)

  var PRINT = false

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
    case m @ MousePressed(_, pt, _, _, _) =>
      val alt = m.peer.isAltDown
      dymaxion.play()
      process(pt, quant = alt)
    case m @ MouseDragged(_, pt, mod) =>
      val alt = m.peer.isAltDown
      process(pt, quant = alt)
    case MouseReleased(_, _, _, _, _) =>
      dymaxion.stop()
  }

  private var lastPrinted = (-999.0, -999.0)

  private def process(pt: Point, quant: Boolean): Unit = {
    val lon0 = pt.x.linlin(0, w, -180, 180)
    val lat0 = pt.y.linlin(0, h,   90, -90)
    val lat  = if (quant) lat0.roundTo(2.5) else lat0
    val lon  = if (quant) lon0.roundTo(2.5) else lon0
    val tt   = f"lat = $lat%1.2f, lon = $lon%1.2f"
    tooltip  = tt
    if (quant) {
      val pair = (lat, lon)
      if (lastPrinted != pair) {
        println(tt)
        lastPrinted = pair
      }
    }

    val latLon = LatLon(lon = lon, lat = lat)
    val idx = Dymaxion.findFaceIndex(latLon)
    val h0  = Dymaxion.mapLonLat    (latLon)
    // val (h0x, h0y) = Dymaxion.mapCartesian(Dymaxion.center(idx))
    // mkPt(pt.x, pt.y)
    // repaint(new Rectangle(pt.x, pt.y, 1, 1))
    if (PRINT) println(f"idx = $idx, x = ${h0.x}%1.2f, y = ${h0.y}%1.2f")

    dymaxion.mark = Some((h0, Radians.North))
  }

  def mkPt(x: Int, y: Int): Unit = {
    val lon = x.linlin(0, w, -180, 180)
    val lat = y.linlin(0, h,   90, -90)
    val idx = Dymaxion.findFaceIndex(LatLon(lon = lon, lat = lat))
    val colr = Color.getHSBColor(idx.linlin(0, 19, 0.0f, 1.0f), 1f, 1f)
    testG.setColor(colr)
    testG.fillRect(x, y, 1, 1)
  }
}

/*
 *  DymaxionView.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package turbulence

import java.awt.geom.{Ellipse2D, Path2D, GeneralPath}
import java.awt.{RenderingHints, BasicStroke, Cursor, Color}
import javax.swing.ImageIcon

import at.iem.sysson.turbulence.Turbulence.{Radians, DymGrid}
import de.sciss.lucre.synth.{Escape, Txn, Synth}
import de.sciss.mellite.Mellite
import de.sciss.synth.{addToTail, SynthGraph}
import at.iem.sysson.turbulence.Dymaxion.DymPt

import scala.concurrent.stm.{TxnExecutor, Ref}
import scala.swing.event.{MouseDragged, MouseReleased, MousePressed, MouseMoved}
import scala.swing.{Point, Swing, Graphics2D, Component}
import Swing._

class DymaxionView extends Component {
  // import DymaxionView._
  import Dymaxion.{hScale, vScale}

  private var _drawImage      = true
  private var _drawSpkr       = true
  private val DRAW_SPKR_IDX   = true

  // private val image = Toolkit.getDefaultToolkit.createImage(getClass.getClassLoader.getResource("dymaxion.png"))
  private val url         = getClass.getResource("dymaxion.png")
  private val image       = new ImageIcon(url)

  private val hNum        = 13
  private val vNum        = 9 // 3
  private val vNum1       = vNum / 2

  private val numCols     = hNum  + 1
  private val numRows     = vNum1 + 1

  private val gainRadius  = 16
  private val w           = hNum * hScale  // 1196
  private val w1          = w + gainRadius + gainRadius
  private val h           = (vNum * vScale + 0.5f).toInt //  480
  private val h1          = h + gainRadius + gainRadius

  // private var tri = ((-1, -1), (-1, -1), (-1, -1))

  private val gpFill      = new GeneralPath(Path2D.WIND_NON_ZERO,  4)
  private val gpStroke    = new GeneralPath(Path2D.WIND_NON_ZERO, 20)
  private val circle      = new Ellipse2D.Float()
  // private val line        = new Line2D.Float()
  private val gpTemp      = new GeneralPath(Path2D.WIND_NON_ZERO,  4)
  private val colrTri     = new Color(0xFF, 0x00, 0x00, 0x7F)
  private val colrSpkr    = new Color(0x00, 0xFF, 0xFF, 0x7F)
  private val colrEmpty   = new Color(0x00, 0x00, 0x00, 0x4F)
  private val colrGain    = Color.blue
  // private val strkGain    = new BasicStroke(2f, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10f, Array(2f, 2f), 0f)
  private val strkGain    = new BasicStroke(2f)

  private var _mark = Option.empty[DymPt]

  private var _crosses = Vec.empty[(DymPt, Radians)]

  def drawImage: Boolean = _drawImage
  def drawImage_=(value: Boolean): Unit = if (_drawImage != value) {
    _drawImage = value
    repaint()
  }

  def drawSpeakers: Boolean = _drawSpkr
  def drawSpeakers_=(value: Boolean): Unit = if (_drawSpkr != value) {
    _drawSpkr = value
    repaint()
  }

  def mark: Option[DymPt] = _mark
  def mark_=(value: Option[DymPt]): Unit = if (_mark != value) {
    _mark = value
    // repaint()
    value.fold(repaint()) { pt =>
      markUpdated(pt.x, pt.y)
    }
  }

  def crosses: Vec[(DymPt, Radians)] = _crosses
  def crosses_=(value: Vec[(DymPt, Radians)]): Unit = {
    _crosses = value
    repaint()
  }

  private var _mouseCtl = false
  def mouseControl: Boolean = _mouseCtl
  def mouseControl_=(value: Boolean): Unit = if (_mouseCtl != value) {
    _mouseCtl = value
    if (value) {
      listenTo(mouse.clicks)
      listenTo(mouse.moves)
    } else {
      deafTo(mouse.clicks)
      deafTo(mouse.moves)
    }
  }

  private var _highQ = true
  def highQuality: Boolean = _highQ
  def highQuality_=(value: Boolean): Unit = if (_highQ != value) {
    _highQ = value
    repaint()
  }

  override protected def paintComponent(g: Graphics2D): Unit = {
    if (_highQ) {
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    }

    if (_drawImage) {
      g.setColor(Color.gray)
      g.fillRect(0, 0, w1, h1)
      image.paintIcon(peer, g, gainRadius, gainRadius)
    }

    if (_drawSpkr) {
      var vx = 0; while (vx < numCols) {
        var vyi = 0; while (vyi < numRows) {
          val vy  = vyi * 2 + (vx % 2)
          val xp  = vx * hScale + gainRadius
          val yp  = vy * vScale + gainRadius
          circle.setFrameFromCenter(xp, yp, xp + 12, yp + 12)
          val chanOpt = Turbulence.MatrixToChannelMap.get(DymGrid(vx, vyi))
          g.setColor(if (chanOpt.isDefined) colrSpkr else colrEmpty)
          g.fill(circle)
          if (DRAW_SPKR_IDX) {
            g.setColor(Color.black)
            g.drawString(s"$vx,$vyi", xp + 10, yp - 5)
            g.setColor(Color.white)
            g.drawString(s"$vx,$vyi", xp +  9, yp - 6)
            chanOpt.foreach { ch =>
              g.setColor(Color.blue)
              val s = ch.num.toString
              g.drawString(s, xp + (if (s.length == 1) -4 else -7), yp + 4)
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

    if (_mark.isDefined) _mark.foreach { case dp =>
      val p   = dp.equalize
      val xp  = dp.x + gainRadius
      val yp  = dp.y + gainRadius
      circle.setFrameFromCenter(xp, yp, xp + 12, yp + 12)
      g.setColor(Color.yellow)
      g.fill(circle)
    }

    if (_crosses.nonEmpty) {
      g.setColor(Color.red)
      val rad  = 8 // 16
      val angD = 10.toRadians
      _crosses.foreach { case (dp, ang) =>
        val p   = dp.equalize
        val xp  = p.x + gainRadius
        val yp  = p.y + gainRadius
        // line.setLine(xp - rad, yp - rad, xp + rad, yp + rad)
        val angL = ang.value - angD
        gpTemp.reset()
        gpTemp.moveTo(xp - math.cos(angL) * rad, yp + math.sin(angL) * rad)
        gpTemp.lineTo(xp, yp)
        // line.setLine(xp - rad, yp + rad, xp + rad, yp - rad)
        val angR = ang.value + angD
        gpTemp.lineTo(xp - math.cos(angR) * rad, yp + math.sin(angR) * rad)
        g.draw(gpTemp)
      }
    }
  }

  preferredSize = (w1, h1)
  cursor = Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR)

  // cf. https://stackoverflow.com/questions/8043947/indexing-in-equilateral-triangle-grid-given-simple-2d-cartesian-coordinates
  private def indices(xf0: Double, yf0: Double): ((Int, Int, Float), (Int, Int, Float), (Int, Int, Float)) = {
    val xf  = math.max(0.0, xf0) // math.min(hNum, xf0))
    val yf  = math.max(0.9, yf0) // math.min(vNum1, yf0))
    val x   = xf.toInt
    val u   = xf - x
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
    def dist(x: Double, y: Double)(lx1: Int, ly1: Int, lx2: Int, ly2: Int): Double = {
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
    val g1 = math.sqrt(d1).toFloat
    val g2 = math.sqrt(d2).toFloat
    val g3 = math.sqrt(d3).toFloat
    // println(f"gain1 = $g1%1.2f, gain2 = $g2%1.2f, gain3 = $g3%1.2f, power-sum = ${math.sqrt(g1*g1 + g2*g2 + g3*g3)}%1.2f")

    ((vx1, vy1i, g1), (vx2, vy2i, g2), (vx3, vy3i, g3))
  }

  private val synthRef = Ref(Option.empty[Synth])

  private def atomic[A](fun: Txn => A): A = TxnExecutor.defaultAtomic { itx =>
    val tx = Txn.wrap(itx)
    fun(tx)
  }

  private def setSynth(opt: Option[Synth])(implicit tx: Txn): Unit =
    atomic { implicit tx =>
      synthRef.swap(opt)(tx.peer).foreach(_.free())
    }

  reactions += {
    case MousePressed(_, pt, _, _, _) =>
      play()
      mouseMoved(pt)

    case MouseReleased(_, pt, _, _, _) =>
      stop()

    case MouseMoved  (_, pt, _) => mouseMoved(pt)
    case MouseDragged(_, pt, _) => mouseMoved(pt)
  }

  def play(): Unit = atomic { implicit tx =>
    val synthOpt = Mellite.auralSystem.serverOption.map { s =>
      val graph = SynthGraph {
        import de.sciss.synth._
        import ugen._
        // val sig = PinkNoise.ar(0.5)
        val sig = Dust.ar(400)
        for (i <- 1 to 3) {
          val bus   = s"c$i".ar(0f)
          val gain  = s"g$i".ar(0f)
          Out.ar(bus, sig * gain)
        }
      }
      val df    = Escape.getSynthDef(s, graph, None)
      val synth = Synth.expanded(s, df.peer.graph)
      synth.play(s.defaultGroup, Nil, addToTail, Nil)
      synth
    }
    setSynth(synthOpt)
  }

  def stop(): Unit = atomic { implicit tx =>
    setSynth(None)
  }

  private def mouseMoved(pt: Point): Unit = {
    val xf = (pt.getX - gainRadius) / hScale
    val yf = (pt.getY - gainRadius) / vScale
    markUpdated(xf, yf)
  }

  private def markUpdated(xf: Double, yf: Double): Unit = {
    val ((vx1, vy1i, g1), (vx2, vy2i, g2), (vx3, vy3i, g3)) = indices(xf, yf)

    def move(x: Int, yi: Int, gain: Double, first: Boolean): Unit = {
      val y   = yi * 2 + (x % 2)
      val xp  = x * hScale + gainRadius
      val yp  = y * vScale + gainRadius
      if (first) gpFill.moveTo(xp, yp) else gpFill.lineTo(xp, yp)
      val g = gain * 12
      circle.setFrameFromCenter(xp, yp, xp + g, yp + g)
      gpStroke.append(circle, false)
    }

    atomic { implicit tx =>
      synthRef.get(tx.peer).foreach { synth =>
        val c1Opt = Turbulence.MatrixToChannelMap.get(DymGrid(vx1, vy1i))
        val c1    = c1Opt.map(_.toIndex).getOrElse(0)
        val c2Opt = Turbulence.MatrixToChannelMap.get(DymGrid(vx2, vy2i))
        val c2    = c2Opt.map(_.toIndex).getOrElse(0)
        val c3Opt = Turbulence.MatrixToChannelMap.get(DymGrid(vx3, vy3i))
        val c3    = c3Opt.map(_.toIndex).getOrElse(0)
        val g1b   = if (c1Opt.isEmpty) 0f else g1
        val g2b   = if (c2Opt.isEmpty) 0f else g2
        val g3b   = if (c3Opt.isEmpty) 0f else g3
        // println(s"""set("c1" -> $c1, "g1" -> $g1b, "c2" -> $c2, "g2" -> $g2b, "c3" -> $c3, "g3" -> $g3b)""")
        synth.set(true, "c1" -> c1, "g1" -> g1b, "c2" -> c2, "g2" -> g2b, "c3" -> c3, "g3" -> g3b)
      }
    }

    gpFill  .reset()
    gpStroke.reset()
    move(vx1, vy1i, g1, first = true )
    move(vx2, vy2i, g2, first = false)
    move(vx3, vy3i, g3, first = false)
    // repaint(gp.getBounds)
    repaint()
  }
}

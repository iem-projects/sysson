package at.iem.sysson.sound.designer.impl

import prefuse.render.{Renderer, AbstractShapeRenderer}
import java.awt.geom.Rectangle2D
import prefuse.util.ColorLib
import java.awt.{Graphics2D, Shape, BasicStroke, Color}
import prefuse.visual.VisualItem

private[impl] object BoxRenderer {
  private final val MIN_BOX_WIDTH = 24
  private final val BOX_HEIGHT    = 18

  private final val colrSel     = Style.selectionColor
  private final val strkColrOk  = ColorLib.getColor(192, 192, 192)
  private final val strkColrEdit= colrSel
  private final val strkColrErr = ColorLib.getColor(240,   0,   0)
  private final val fillColr    = Style.boxColor
  private final val textColrEdit= strkColrEdit
  private final val textColr    = Color.black
  private final val strkShpOk   = new BasicStroke(1f)
  private final val strkShpPend = new BasicStroke(1f, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10f, Array[Float](6, 4), 0f)
  private final val portColr    = ColorLib.getColor( 80,  80, 128)
}
private[impl] final class BoxRenderer(d: DesignerViewImpl) extends AbstractShapeRenderer {
  import BoxRenderer._

  private val r   = new Rectangle2D.Float()
  private val r2  = new Rectangle2D.Float()

  protected def getRawShape(vi: VisualItem): Shape = {
    var x    = vi.getX
    if (x.isNaN || x.isInfinity) x = 0.0
    var y    = vi.getY
    if (y.isNaN || y.isInfinity) y = 0.0

    val w = d.getData(vi) match {
      case Some(data) =>
        val fm    = Renderer.DEFAULT_GRAPHICS.getFontMetrics(Style.font)
        math.max(MIN_BOX_WIDTH, fm.stringWidth(data.name) + 6)
      case _ => MIN_BOX_WIDTH
    }

    r.setRect(x, y, w, BOX_HEIGHT)
    r
  }

  override def render(g: Graphics2D, vi: VisualItem) {
    d.getData(vi).foreach { data =>
      val r = getShape(vi)
      val b = r.getBounds2D
      g.setColor(fillColr)
      g.fill(r)
      data.state match {
        case ElementState.Ok =>
          g.setColor(strkColrOk)
          g.setStroke(strkShpOk)
        case ElementState.Edit =>
          g.setColor(strkColrEdit)
          g.setStroke(strkShpPend)
        case ElementState.Error =>
          g.setColor(strkColrErr)
          g.setStroke(strkShpPend)
      }
      g.draw(r)
      data.state match {
        case ElementState.Edit =>
          g.setColor(textColrEdit)
        case _ =>
          g.setColor(textColr)
      }
      g.setFont(Style.font)
      val fm  = Renderer.DEFAULT_GRAPHICS.getFontMetrics(Style.font)
      val x   = b.getX.toFloat
      val y   = b.getY.toFloat
      g.drawString(data.name, x + 3, y + 2 + fm.getAscent)

      data match {
        case vge: VisualGraphElem =>
          vge.content.foreach { ge =>
            d.getPorts(vi).foreach { ports =>
              val atOrig = g.getTransform
              g.translate(x, y)
              g.setColor(portColr)
              ports.inlets .foreach(g.fill(_))
              ports.outlets.foreach(g.fill(_))
              ports.active.foreach { p =>
                val r = p.visualRect(ports)
                g.setColor(colrSel)
                r2.setRect(r.getX - 1, r.getY - 1, r.getWidth + 2, r.getHeight + 2)
                g.fill(r2)
              }
              g.setTransform(atOrig)
            }
          }
        case vc: VisualConstant =>

      }
    }
  }
}

package at.iem.sysson.sound.designer.impl

import java.awt.event.MouseEvent
import prefuse.controls.ControlAdapter
import java.awt.geom.{Ellipse2D, Area, Rectangle2D, Point2D}
import prefuse.visual.VisualItem
import collection.immutable.{IndexedSeq => IIdxSeq}
import java.awt.{Point, Toolkit, Color, RenderingHints, Cursor}
import java.awt.image.BufferedImage
import javax.swing.SwingUtilities

// TODO: add TableListener to react to items disappearing (see original DragControl)
object DragControl {
  private val csrPatch = {
    val img = new BufferedImage(17, 17, BufferedImage.TYPE_INT_ARGB)
    val g   = img.createGraphics()
    val shp1 =    new Area(new Ellipse2D.Float(0, 0, 17, 17))
    shp1.subtract(new Area(new Ellipse2D.Float(5, 5,  7,  7)))
    val shp2 =    new Area(new Ellipse2D.Float(1, 1, 15, 15))
    shp2.subtract(new Area(new Ellipse2D.Float(4, 4,  9,  9)))
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setColor(Color.white)
    g.fill(shp1)
    g.setColor(Color.black)
    g.fill(shp2)
    g.dispose()
    Toolkit.getDefaultToolkit.createCustomCursor(img, new Point(8, 8), "patch")
  }
}
final class DragControl(d: DesignerViewImpl) extends ControlAdapter {
  import DragControl._

  val mousePoint = new Point2D.Float()  // in virtual space
  private var activeItem  = Option.empty[VisualItem]
  private var activePort  = Option.empty[Port]

  private val dragPoint   = new Point2D.Float
  private var dragPressed = false
  private var dragStarted = false
  private var dragPort    = Option.empty[Port]

  private val dragTemp    = new Point2D.Float

  private def reportMouse(e: MouseEvent) {
    val at = d.display.getInverseTransform
    at.transform(e.getPoint, mousePoint)
//println(s"mouse screen ${e.getPoint} - virt ${mousePoint}")
  }

  private def findPort(seq: IIdxSeq[Rectangle2D], tx: Double, ty: Double): Int = seq.indexWhere { r =>
    r.getMinX - 1 <= tx && r.getMaxX >= tx && r.getMinY - 1 <= ty && r.getMaxY >= ty
  }

  private def detectPort(ports: VisualPorts, vi: VisualItem, e: MouseEvent): Option[Port] = {
    val b     = vi.getBounds
    val tx    = mousePoint.getX - b.getX
    val ty    = mousePoint.getY - b.getY
    val idxIn = findPort(ports.inlets, tx, ty)
    if (idxIn >= 0) Some(Port.In(idxIn)) else {
      val idxOut = findPort(ports.outlets, tx, ty)
      if (idxOut >= 0) Some(Port.Out(idxOut)) else None
    }
  }

  private def processMove(vi: VisualItem, e: MouseEvent) {
    reportMouse(e)
    d.getPorts(vi).foreach { ports =>
      activePort = detectPort(ports, vi, e)
      if (ports.active != activePort) {
        ports.active = activePort
//println("SET " + ports.active)
        vi.setValidated(false)  // force repaint
        d.visualization.repaint()
      }
    }
  }

  private def updateCursor() {
    d.display.setCursor(if (activePort.isDefined) csrPatch else Cursor.getPredefinedCursor(Cursor.HAND_CURSOR))
  }

  override def itemEntered(vi: VisualItem, e: MouseEvent) {
//        if (e.isConsumed) return
//        println("DRAG INVOKED")
    activeItem = Some(vi)
    processMove(vi, e)
    updateCursor()
  }

  override def itemExited(vi: VisualItem, e: MouseEvent) {
    activeItem  = None
    if (activePort.isDefined) {
      activePort  = None
      d.getPorts(vi).foreach(_.active = None)
      d.visualization.repaint()
    }
    d.display.setCursor(Cursor.getDefaultCursor)
//        processMove(vi, e)
  }

  override def itemMoved(vi: VisualItem, e: MouseEvent) {
    val hadPort = activePort.isDefined
    processMove(vi, e)
    if (activePort.isDefined != hadPort) updateCursor()
  }

  override def itemPressed(vi: VisualItem, e: MouseEvent) {
    if (!SwingUtilities.isLeftMouseButton(e)) return
    // d.display.getAbsoluteCoordinate(e.getPoint(), down)

    if (e.getClickCount == 2 && activePort.isEmpty) {
      d.editObject(vi)
    } else {
      dragPressed = true
      dragPort    = activePort
      dragStarted = false
      d.display.getAbsoluteCoordinate(e.getPoint, dragPoint)
    }
  }

  override def itemReleased(vi: VisualItem, e: MouseEvent) {
    if (!SwingUtilities.isLeftMouseButton(e) || !dragPressed) return

    dragPressed = false
    dragStarted = false
  }

  override def itemDragged(vi: VisualItem, e: MouseEvent ) {
    if (!SwingUtilities.isLeftMouseButton(e) || !dragPressed) return

    d.display.getAbsoluteCoordinate(e.getPoint, dragTemp)
    val dx  = dragTemp.getX - dragPoint.getX
    val dy  = dragTemp.getY - dragPoint.getY

    if (!dragStarted) {
      val dist = dx * dx + dy * dy
      if (dist < 4) return
    }

    if (dragPort.isDefined) return

    val x   = vi.getX
    val y   = vi.getY

    vi.setStartX(x)
    vi.setStartY(y)
    vi.setX     (x + dx)
    vi.setY     (y + dy)
    vi.setEndX  (x + dx)
    vi.setEndY  (y + dy)

    vi.getVisualization.repaint()

    dragPoint.setLocation(dragTemp)
//      if ( action != null )
//          d.getVisualization.run(action)
  }

  override def mouseEntered(e: MouseEvent) { reportMouse(e) }
  override def mouseDragged(e: MouseEvent) { reportMouse(e) }
  override def mouseMoved(  e: MouseEvent) { reportMouse(e) }
}
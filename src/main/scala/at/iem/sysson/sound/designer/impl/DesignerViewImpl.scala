package at.iem.sysson
package sound
package designer
package impl

import collection.immutable.{IndexedSeq => IIdxSeq}
import collection.breakOut
import prefuse.{Visualization, Display}
import swing.{Action, Component, Frame}
import javax.swing.JComponent
import gui.GUI
import java.awt.event.{MouseEvent, MouseMotionAdapter, KeyEvent}
import prefuse.util.display.PaintListener
import java.awt.{BasicStroke, Color, Shape, Graphics2D}
import prefuse.data.Graph
import prefuse.controls.{ZoomingPanControl, PanControl, DragControl, ControlAdapter}
import javax.swing.event.MouseInputAdapter
import java.awt.geom.{Rectangle2D, Point2D}
import prefuse.action.{RepaintAction, ActionList}
import prefuse.render.{AbstractShapeRenderer, LabelRenderer, DefaultRendererFactory}
import prefuse.util.ColorLib
import prefuse.visual.VisualItem

object DesignerViewImpl {
  def apply(): DesignerView = new Impl

  object Input {
    sealed trait Type
    case object GraphElem extends Type

    final case class Spec(name: String, tpe: Type = GraphElem, default: Option[Constant] = None)
  }
  sealed trait Input

  object GraphElem {
    final case class Spec(name: String, inputs: IIdxSeq[Input.Spec]) {
      def mkInputSlots: IIdxSeq[Option[Input]] = inputs.map(_.default)
    }
  }
  final class GraphElem(val spec: GraphElem.Spec) extends Input {
    def name        = spec.name
    var inputElems  = spec.mkInputSlots
  }

  final case class Constant(values: IIdxSeq[Float]) extends Input

  object ElementState {
    case object New extends ElementState
    case object Error extends ElementState
    case object Ok extends ElementState
  }
  sealed trait ElementState

  sealed trait VisualElement {
    var name: String = ""
    var state: ElementState = ElementState.New
  }
  final class VisualGraphElem extends VisualElement {
    var content = Option.empty[GraphElem]
  }
  final class VisualConstant extends VisualElement {
    var content = Option.empty[Constant]
  }

  private implicit def mkCont(value: Float): Constant = Constant(IIdxSeq(value))

  object Collection {
    val all = IIdxSeq(
      GraphElem.Spec("SinOsc", IIdxSeq(
        Input.Spec("freq",  default = Some(440f)),
        Input.Spec("phase", default = Some(  0f))
      ))
    )

    val map: Map[String, GraphElem.Spec] = all.map(e => e.name -> e)(breakOut)
  }

//  private object Mode {
////    case object Select extends Mode
////    case object PutObject extends Mode
////    case object
//  }
//  private sealed trait Mode

  private final val GROUP_GRAPH   = "graph"
  private final val GROUP_NODES   = "graph.nodes"
  private final val GROUP_EDGES   = "graph.edges"
  private final val COL_ELEM      = "element"

  private final val MIN_BOX_WIDTH = 24
  private final val BOX_HEIGHT    = 18

  private final val ACTION_REPAINT  = "repaint"

  private object BoxRenderer extends AbstractShapeRenderer {
    private val r = new Rectangle2D.Float()

    val strkColrOk  = ColorLib.getColor(192, 192, 192)
    val strkColrNew = ColorLib.getColor(  0,   0, 240)
    val strkColrErr = ColorLib.getColor(240,   0,   0)
    val fillColr    = ColorLib.getColor(246, 248, 248)
    val textColr    = Color.black
    val strkShpOk   = new BasicStroke(1f)
    val strkShpPend = new BasicStroke(1f, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10f, Array[Float](6, 4), 0f)

    protected def getRawShape(vi: VisualItem): Shape = {
      var x    = vi.getX
      if (x.isNaN || x.isInfinity) x = 0.0
      var y    = vi.getY
      if (y.isNaN || y.isInfinity) y = 0.0
      r.setRect(x, y, MIN_BOX_WIDTH, BOX_HEIGHT)
      r
    }

    override def render(g: Graphics2D, vi: VisualItem) {
      val data = vi.get(COL_ELEM).asInstanceOf[VisualElement]
      if (data == null) return
      val r = getShape(vi)
      g.setColor(fillColr)
      g.fill(r)
      data.state match {
        case ElementState.Ok =>
          g.setColor(strkColrOk)
          g.setStroke(strkShpOk)
        case ElementState.New =>
          g.setColor(strkColrNew)
          g.setStroke(strkShpPend)
        case ElementState.Error =>
          g.setColor(strkColrErr)
          g.setStroke(strkShpPend)
      }
      g.draw(r)
     }
  }

  private final class Impl extends DesignerView {
//    private var mode: Mode = Mode.Select

    val vis   = new Visualization
    val disp  = {
      val res   = new Display(vis)
      val imap  = res.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
      val amap  = res.getActionMap
      import GUI.{meta, stroke}
      import KeyEvent._
      imap.put(stroke(VK_1, meta), "designer.putObject")
      amap.put("designer.putObject", Action("putObject") {
        enterPutObject()
      }.peer)
      res
    }
    val g   = new Graph
    g.addColumn(COL_ELEM, classOf[VisualElement])
    val vg  = vis.addGraph(GROUP_GRAPH, g)

//    disp.addPaintListener(new PaintListener {
//      def prePaint(p1: Display, p2: Graphics2D) {
//        sys.error("TODO")
//      }
//
//      def postPaint(p1: Display, p2: Graphics2D) {
//        sys.error("TODO")
//      }
//    })

//    disp.addControlListener(new ControlAdapter {
//
//    })

    private val mousePoint = new Point2D.Float()

    def reportMouse(e: MouseEvent) {
      val t     = disp.getTransform
      t.transform(e.getPoint, mousePoint)
//println(s"mouse screen ${e.getPoint} - virt ${mousePoint}")
    }

    // ControlAdapter branches into all sorts
    // of methods when mouse is over visual item,
    // so a plain mouse listener is more useful here
    val mouseCoordListener = new MouseInputAdapter {
      override def mouseDragged(e: MouseEvent) {
        reportMouse(e)
      }
      override def mouseMoved(e: MouseEvent) {
        reportMouse(e)
      }
      override def mouseEntered(e: MouseEvent) {
        reportMouse(e)
      }
    }

    disp.addMouseListener(mouseCoordListener)
    disp.addMouseMotionListener(mouseCoordListener)
    disp.addControlListener(new DragControl())
    disp.addControlListener(new ZoomingPanControl())

    val rf = new DefaultRendererFactory(BoxRenderer)
    vis.setRendererFactory(rf)

    val frame = new Frame {
      title     = "Sound Designer"
      contents  = Component.wrap(disp)
      pack()
      centerOnScreen()
      open()
    }

//    vis.putAction(ACTION_REPAINT, new RepaintAction())

    def enterPutObject() {
      val n   = g.addNode()
      n.set(COL_ELEM, new VisualGraphElem)
      val vi  = vis.getVisualItem(GROUP_GRAPH, n)
//      vi.setBounds(mousePoint.getX, mousePoint.getY, MIN_BOX_WIDTH, BOX_HEIGHT)
      vi.setX(mousePoint.getX - 2)
      vi.setY(mousePoint.getY - 2)
//      vi.setFont()
//      vi.setStrokeColor(ColorLib.gray(0))
      vis.repaint()
    }
  }
}
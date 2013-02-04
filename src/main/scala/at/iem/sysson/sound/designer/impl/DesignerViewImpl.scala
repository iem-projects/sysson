package at.iem.sysson
package sound
package designer
package impl

import collection.immutable.{IndexedSeq => IIdxSeq}
import collection.breakOut
import prefuse.{Visualization, Display}
import swing.{Action, Component, Frame}
import javax.swing.{JTextField, JComponent}
import gui.GUI
import java.awt.event.{ActionEvent, ActionListener, MouseEvent, MouseMotionAdapter, KeyEvent}
import prefuse.util.display.PaintListener
import java.awt.{GraphicsEnvironment, Font, Rectangle, BasicStroke, Color, Shape, Graphics2D}
import prefuse.data.Graph
import prefuse.controls.{ZoomControl, ZoomingPanControl, PanControl, DragControl, ControlAdapter}
import javax.swing.event.{DocumentEvent, DocumentListener, MouseInputAdapter}
import java.awt.geom.{Rectangle2D, Point2D}
import prefuse.action.{RepaintAction, ActionList}
import prefuse.render.{Renderer, AbstractShapeRenderer, LabelRenderer, DefaultRendererFactory}
import prefuse.util.ColorLib
import prefuse.visual.VisualItem
import javax.swing.text.JTextComponent

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
    case object Edit  extends ElementState
    case object Error extends ElementState
    case object Ok    extends ElementState
  }
  sealed trait ElementState

  sealed trait VisualElement {
    var name: String = ""
    var state: ElementState = ElementState.Edit
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

//  private object TextEditor extends JTextField() {
//    setBorder(null)
//    setVisible(false)
//    addActionListener(new ActionListener {
//      def actionPerformed(e: ActionEvent) {
//        println("JUHU " + getText)
//      }
//    })
//  }

  private val font = {
    val fntNames  = GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames
    val fntMenlo  = "Menlo"
    val name      = if (fntNames.contains(fntMenlo)) {
      fntMenlo
    } else {
      Font.MONOSPACED
    }
    new Font(name, Font.PLAIN, 11)
  }

  private final class Impl extends DesignerView {
//    private var mode: Mode = Mode.Select

    var editingNode     = Option.empty[VisualItem]
    var editingOldText  = ""

    private object BoxRenderer extends AbstractShapeRenderer {
      private val r = new Rectangle2D.Float()

      val strkColrOk  = ColorLib.getColor(192, 192, 192)
      val strkColrEdit= ColorLib.getColor(  0,   0, 240)
      val strkColrErr = ColorLib.getColor(240,   0,   0)
      val fillColr    = ColorLib.getColor(246, 248, 248)
      val textColrEdit= strkColrEdit
      val textColr    = Color.black
      val strkShpOk   = new BasicStroke(1f)
      val strkShpPend = new BasicStroke(1f, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10f, Array[Float](6, 4), 0f)

      protected def getRawShape(vi: VisualItem): Shape = {
        var x    = vi.getX
        if (x.isNaN || x.isInfinity) x = 0.0
        var y    = vi.getY
        if (y.isNaN || y.isInfinity) y = 0.0

        val w = getData(vi) match {
          case Some(data) =>
            val fm    = Renderer.DEFAULT_GRAPHICS.getFontMetrics(font)
            math.max(MIN_BOX_WIDTH, fm.stringWidth(data.name) + 6)
          case _ => MIN_BOX_WIDTH
        }

        r.setRect(x, y, w, BOX_HEIGHT)
        r
      }

      override def render(g: Graphics2D, vi: VisualItem) {
        getData(vi).foreach { data =>
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
          g.setFont(font)
          val fm = Renderer.DEFAULT_GRAPHICS.getFontMetrics(font)
          g.drawString(data.name, b.getX.toFloat + 3, b.getY.toFloat + 2 + fm.getAscent)
        }
      }
    }

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
      val at = disp.getInverseTransform
      at.transform(e.getPoint, mousePoint)
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
//    disp.addControlListener(new ZoomingPanControl())
    disp.addControlListener(new PanControl())
    disp.addControlListener(new ZoomControl())
//    disp.setTextEditor(TextEditor)

    def getData(vi: VisualItem): Option[VisualElement] = Option(vi.get(COL_ELEM).asInstanceOf[VisualElement])

    disp.getTextEditor match {
      case tf: JTextField =>
        tf.setFont(font)
        tf.setForeground(BoxRenderer.strkColrEdit)
        tf.setBackground(BoxRenderer.fillColr)
        tf.getDocument.addDocumentListener(new DocumentListener {
          def refreshBox() {
            editingNode.foreach { vi =>
              getData(vi).foreach { data =>
                data.name = tf.getText
//                vi.set(COL_ELEM, data)
//                vis.repaint()
                val r = updateEditingBounds(vi)
//                println("BOUNDS " + r)
                tf.setSize(r.getSize)
                vis.repaint()
              }
            }
          }

          def insertUpdate( e: DocumentEvent) { refreshBox() }
          def removeUpdate( e: DocumentEvent) { refreshBox() }
          def changedUpdate(e: DocumentEvent) { refreshBox() }
        })
        tf.addActionListener(new ActionListener {
          def actionPerformed(e: ActionEvent) {
            disp.stopEditing()
            editingNode.foreach { vi =>
              editingNode = None
              getData(vi).foreach { data =>
                data.name   = tf.getText
                if (data.name != editingOldText) {
                  data.state  = if (Collection.map.contains(data.name)) ElementState.Ok else ElementState.Error
                  vis.repaint()
                }
              }
            }
          }
      })
    }

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

    def updateEditingBounds(vi: VisualItem): Rectangle = {
//      vi.validateBounds()
      vi.setValidated(false)  // this causes a subsequent call to getBounds to ask the renderer again, plus creates dirty screen region
      val b      = vi.getBounds
      val at     = disp.getTransform
      val r      = at.createTransformedShape(b).getBounds
      r.x       += 3
      r.y       += 1
      r.width   -= 5
      r.height  -= 2
      r
    }

    def editObject(vi: VisualItem) {
      getData(vi).foreach { data =>
        val r = updateEditingBounds(vi)
        editingNode     = Some(vi)
        editingOldText  = data.name
        disp.editText("", r)
      }
    }

    def enterPutObject() {
      val n   = g.addNode()
      n.set(COL_ELEM, new VisualGraphElem)
      val vi  = vis.getVisualItem(GROUP_GRAPH, n)
      vi.setX(mousePoint.getX - 2)
      vi.setY(mousePoint.getY - 2)
      editObject(vi)
      vis.repaint()
    }
  }
}
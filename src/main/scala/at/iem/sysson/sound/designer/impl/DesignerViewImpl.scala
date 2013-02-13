package at.iem.sysson
package sound
package designer
package impl

import collection.immutable.{IndexedSeq => IIdxSeq}
import prefuse.{Visualization, Display}
import swing.{BorderPanel, Action, Component, Frame}
import javax.swing.{JTextField, JComponent}
import gui.GUI
import java.awt.event.{ActionEvent, ActionListener, MouseEvent, KeyEvent}
import java.awt.{RenderingHints, Cursor, Point, Toolkit, GraphicsEnvironment, Font, Rectangle, BasicStroke, Color, Shape, Graphics2D}
import prefuse.data.Graph
import prefuse.controls.{ControlAdapter, ZoomControl, PanControl}
import javax.swing.event.{DocumentEvent, DocumentListener}
import java.awt.geom.{Area, Ellipse2D, Rectangle2D, Point2D}
import prefuse.render.{Renderer, AbstractShapeRenderer, DefaultRendererFactory}
import prefuse.util.ColorLib
import prefuse.visual.VisualItem
import de.sciss.audiowidgets.Transport
import java.awt.image.BufferedImage
import de.sciss.synth._

private[designer] object DesignerViewImpl {
  def apply(): DesignerView = new View

  private object ElementState {
    case object Edit  extends ElementState
    case object Error extends ElementState
    case object Ok    extends ElementState
  }
  private sealed trait ElementState

  private sealed trait VisualElement {
    var name: String = ""
    var state: ElementState = ElementState.Edit
  }
  private final class VisualGraphElem extends VisualElement {
    var content = Option.empty[GraphElem]
  }
  private final class VisualConstant extends VisualElement {
    var content = Option.empty[ConstElem]
  }

  private object Collection {
    val map = UGenSpec.standardUGens
  }

  private final val GROUP_GRAPH   = "graph"
//  private final val GROUP_NODES   = "graph.nodes"
//  private final val GROUP_EDGES   = "graph.edges"
  private final val COL_ELEM      = "element"
  private final val COL_PORTS     = "ports"

  private final val MIN_BOX_WIDTH = 24
  private final val BOX_HEIGHT    = 18

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

  private final class View extends DesignerViewImpl {
//    private var mode: Mode = Mode.Select

    private var editingNode     = Option.empty[VisualItem]
    private var editingOldText  = ""

    private object BoxRenderer extends AbstractShapeRenderer {
      private val r   = new Rectangle2D.Float()
      private val r2  = new Rectangle2D.Float()

      val colrSel     = ColorLib.getColor(  0,   0, 240)
      val strkColrOk  = ColorLib.getColor(192, 192, 192)
      val strkColrEdit= colrSel
      val strkColrErr = ColorLib.getColor(240,   0,   0)
      val fillColr    = ColorLib.getColor(246, 248, 248)
      val textColrEdit= strkColrEdit
      val textColr    = Color.black
      val strkShpOk   = new BasicStroke(1f)
      val strkShpPend = new BasicStroke(1f, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10f, Array[Float](6, 4), 0f)
      val portColr    = ColorLib.getColor( 80,  80, 128)

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
          val fm  = Renderer.DEFAULT_GRAPHICS.getFontMetrics(font)
          val x   = b.getX.toFloat
          val y   = b.getY.toFloat
          g.drawString(data.name, x + 3, y + 2 + fm.getAscent)

          data match {
            case vge: VisualGraphElem =>
              vge.content.foreach { ge =>
                getPorts(vi).foreach { ports =>
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

    private val dragControl = new DragControl(this)
    val visualization = new Visualization
    val display  = {
      val res   = new Display(visualization)
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
    private val g   = new Graph
    g.addColumn(COL_ELEM, classOf[VisualElement])
    private val vg  = visualization.addGraph(GROUP_GRAPH, g)
    vg.addColumn(COL_PORTS, classOf[VisualPorts])

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

//    // ControlAdapter branches into all sorts
//    // of methods when mouse is over visual item,
//    // so a plain mouse listener is more useful here
//    val mouseCoordListener = new MouseInputAdapter {
//      override def mouseDragged(e: MouseEvent) {
//        reportMouse(e)
//      }
//      override def mouseMoved(e: MouseEvent) {
//        reportMouse(e)
//      }
//      override def mouseEntered(e: MouseEvent) {
//        reportMouse(e)
//      }
//    }

//    disp.addMouseListener(mouseCoordListener)
//    disp.addMouseMotionListener(mouseCoordListener)
//    disp.addControlListener(TestControl)
    display.addControlListener(dragControl)
//    disp.addControlListener(new DragControl())
//    disp.addControlListener(new ZoomingPanControl())
    display.addControlListener(new PanControl())
    display.addControlListener(new ZoomControl())
//    disp.setTextEditor(TextEditor)

//    object TestControl extends ControlAdapter {
//      override def itemEntered(vi: VisualItem, e: MouseEvent) {
//        if (e.isAltDown) {
//          println("TEST CONSUME")
//          e.consume()
//        }
//      }
//    }

    def getData( vi: VisualItem): Option[VisualElement] = Option(vi.get(COL_ELEM ).asInstanceOf[VisualElement])
    def getPorts(vi: VisualItem): Option[VisualPorts  ] = Option(vi.get(COL_PORTS).asInstanceOf[VisualPorts  ])

    private def stopEditing() {
      val txt = display.getTextEditor.getText
      display.stopEditing()
      editingNode.foreach { vi =>
        editingNode = None
        getData(vi).foreach { data =>
          data.name = txt
          data match {
            case vge: VisualGraphElem =>
              if (vge.name != editingOldText) {
                val n = vge.name
                val i = n.indexOf('.')
                val (name, rate) = if (i > 0) {
                  val rOpt = PartialFunction.condOpt(n.substring(i + 1)) {
                    case audio.methodName   => audio
                    case control.methodName => control
                    case scalar.methodName  => scalar
                    case demand.methodName  => demand
                  }
                  rOpt match {
                    case Some(r)  => n.substring(0, i) -> r
                    case _        => "???" -> UndefinedRate
                  }
                } else {
                  n -> UndefinedRate
                }
                Collection.map.get(name) match {
                  case Some(spec) =>
                    vge.state   = ElementState.Ok
                    vge.content = Some(new GraphElem(spec, rate))
                    vi.set(COL_PORTS, new VisualPorts(numIns = spec.args.size, numOuts = spec.outputs.size))
                  case _ =>
                    vge.state   = ElementState.Error
                    vge.content = None
                    vi.set(COL_PORTS, null)
                }
                updateEditingBounds(vi)
                visualization.repaint()
              }

            case vc: VisualConstant =>

          }
        }
      }
    }

    display.getTextEditor match {
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
                visualization.repaint()
              }
            }
          }

          def insertUpdate( e: DocumentEvent) { refreshBox() }
          def removeUpdate( e: DocumentEvent) { refreshBox() }
          def changedUpdate(e: DocumentEvent) { refreshBox() }
        })
        tf.addActionListener(new ActionListener {
          def actionPerformed(e: ActionEvent) {
            stopEditing()
          }
      })
    }

    private val rf = new DefaultRendererFactory(BoxRenderer)
    visualization.setRendererFactory(rf)

    private val transport = Transport.makeButtonStrip {
      import Transport._
      Seq(
        GoToBegin {
          println("Go-to-begin")
        },
        Rewind {
          println("Rewind")
        },
        Stop {
          println("Stop")
        },
        Pause {
          println("Pause")
        },
        Play {
          println("Play")
        },
        FastForward {
          println("Fast-forward")
        },
        Loop {
          println("Loop")
        }
      )
    }

    private val frame = new Frame {
      title     = "Sound Designer"
      contents  = new BorderPanel {
        add(Component.wrap(display), BorderPanel.Position.Center)
        add(transport, BorderPanel.Position.South)
      }
      pack()
      centerOnScreen()
      open()
    }

//    vis.putAction(ACTION_REPAINT, new RepaintAction())

    private def updateEditingBounds(vi: VisualItem): Rectangle = {
//      vi.validateBounds()
      vi.setValidated(false)  // this causes a subsequent call to getBounds to ask the renderer again, plus creates dirty screen region
      val b      = vi.getBounds
      val at     = display.getTransform
      val r      = at.createTransformedShape(b).getBounds
      r.x       += 3
      r.y       += 1
      r.width   -= 5
      r.height  -= 2
      getPorts(vi).foreach(_.update(b))
      r
    }

    private def editObject(vi: VisualItem) {
      getData(vi).foreach { data =>
        val r = updateEditingBounds(vi)
        editingNode     = Some(vi)
        editingOldText  = data.name
        display.editText("", r)
      }
    }

    private def enterPutObject() {
      val n   = g.addNode()
      n.set(COL_ELEM, new VisualGraphElem)
      val vi  = visualization.getVisualItem(GROUP_GRAPH, n)
      val mp  =dragControl.mousePoint
      vi.setX(mp.getX - 2)
      vi.setY(mp.getY - 2)
      editObject(vi)
      visualization.repaint()
    }
  }
}
private[impl] sealed trait DesignerViewImpl extends DesignerView {
  def display: Display
  def visualization: Visualization
  def getPorts(vi: VisualItem): Option[VisualPorts  ]
}
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

object DesignerViewImpl {
  def apply(): DesignerView = new Impl

  object Port {
    final case class In(idx: Int) extends Port {
      def visualRect(ports: VisualPorts)  = ports.inlets(idx)
      def name(elem: GraphElem)           = elem.spec.args(idx).name
    }
    final case class Out(idx: Int) extends Port {
      def visualRect(ports: VisualPorts)  = ports.outlets(idx)
      def name(elem: GraphElem)           = elem.spec.outputs(idx).name.getOrElse("out")
    }
  }
  sealed trait Port {
    def visualRect(ports: VisualPorts): Rectangle2D
    def name(elem: GraphElem): String
  }

//  object Input {
//    sealed trait Type
//    case object GraphElem extends Type
//
//    final case class Spec(name: String, tpe: Type = GraphElem, default: Option[Constant] = None)
//  }
  sealed trait Input
//
//  object GraphElem {
//    final case class Spec(name: String, inputs: IIdxSeq[Input.Spec], outputs: IIdxSeq[String] = IIdxSeq("out")) {
//      def numIns  = inputs.size
//      def numOuts = outputs.size
//      def mkInputSlots:  IIdxSeq[Option[Input]]             = inputs.map(_.default)
//      def mkOutputSlots: IIdxSeq[Option[(GraphElem, Int)]]  = IIdxSeq.fill(numOuts)(None)
//    }
//  }
  final class GraphElem(val spec: UGenSpec, val rate: MaybeRate) extends Input {
    def name                                        = spec.name
    var inputs: IIdxSeq[Option[Input]]              = spec.args.map { a =>
      a.defaults.get(rate).map(v => ConstElem(Vector(v)))
    }
    var outputs: IIdxSeq[Option[(GraphElem, Int)]]  = IIdxSeq.fill(spec.outputs.size)(None)
  }

  final case class ConstElem(values: IIdxSeq[UGenSpec.ArgumentValue]) extends Input

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
    var content = Option.empty[ConstElem]
  }

//  private implicit def mkCont(value: Float): Constant = Constant(IIdxSeq(value))

  object Collection {
//    val all = IIdxSeq(
//      GraphElem.Spec("SinOsc", IIdxSeq(
//        Input.Spec("freq",  default = Some(440f)),
//        Input.Spec("phase", default = Some(  0f))
//      ))
//    )
    val map = UGenSpec.standardUGens

//    val map: Map[String, UGenSpec] = all.map(e => e.name -> e)(breakOut)
  }

//  private object Mode {
////    case object Select extends Mode
////    case object PutObject extends Mode
////    case object
//  }
//  private sealed trait Mode

  private final val GROUP_GRAPH   = "graph"
//  private final val GROUP_NODES   = "graph.nodes"
//  private final val GROUP_EDGES   = "graph.edges"
  private final val COL_ELEM      = "element"
  private final val COL_PORTS     = "ports"

  private final val MIN_BOX_WIDTH = 24
  private final val BOX_HEIGHT    = 18

  final class VisualPorts(numIns: Int, numOuts: Int) {
    val inlets  = IIdxSeq.fill(numIns )(new Rectangle2D.Float)
    val outlets = IIdxSeq.fill(numOuts)(new Rectangle2D.Float)
    var active  = Option.empty[Port]

    def update(bounds: Rectangle2D) {
//      val x       = bounds.getX.toFloat
//      val y       = bounds.getY.toFloat
      val x       = 0f
      val y       = 0f
      val w       = bounds.getWidth.toFloat
      val h       = bounds.getHeight.toFloat
      val wm      = w - 7
      if (numIns > 0) {
        val xf = if (numIns > 1) wm / (numIns - 1) else 0f
        var i = 0; while (i < numIns) {
          inlets(i).setRect(x + i * xf, y, 8, 3)
        i += 1 }
      }
      if (numOuts > 0) {
        val xf = if (numOuts > 1) wm / (numOuts - 1) else 0f
        val y2 = y + h - 2 /* 3 */
        var i = 0; while (i < numOuts) {
          outlets(i).setRect(x + i * xf, y2, 8, 3)
        i += 1 }
      }
    }
  }

//  private final val ACTION_REPAINT  = "repaint"

//  private object TextEditor extends JTextField() {
//    setBorder(null)
//    setVisible(false)
//    addActionListener(new ActionListener {
//      def actionPerformed(e: ActionEvent) {
//        println("JUHU " + getText)
//      }
//    })
//  }

  val font = {
    val fntNames  = GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames
    val fntMenlo  = "Menlo"
    val name      = if (fntNames.contains(fntMenlo)) {
      fntMenlo
    } else {
      Font.MONOSPACED
    }
    new Font(name, Font.PLAIN, 11)
  }

  val csrPatch = {
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

  private final class Impl extends DesignerView {
//    private var mode: Mode = Mode.Select

    var editingNode     = Option.empty[VisualItem]
    var editingOldText  = ""

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
    disp.addControlListener(DragControl)
//    disp.addControlListener(new DragControl())
//    disp.addControlListener(new ZoomingPanControl())
    disp.addControlListener(new PanControl())
    disp.addControlListener(new ZoomControl())
//    disp.setTextEditor(TextEditor)

    // TODO: add TableListener to react to items disappearing (see original DragControl)
    object DragControl extends ControlAdapter {
      val mousePoint = new Point2D.Float()  // in virtual space
      private var activeItem  = Option.empty[VisualItem]
      private var activePort  = Option.empty[Port]

      def reportMouse(e: MouseEvent) {
        val at = disp.getInverseTransform
        at.transform(e.getPoint, mousePoint)
  //println(s"mouse screen ${e.getPoint} - virt ${mousePoint}")
      }

      private def findPort(seq: IIdxSeq[Rectangle2D], tx: Double, ty: Double): Int = seq.indexWhere { r =>
        r.getMinX - 1 <= tx && r.getMaxX >= tx && r.getMinY - 1 <= ty && r.getMaxY >= ty
      }

      def processMove(vi: VisualItem, e: MouseEvent) {
        reportMouse(e)
        getPorts(vi).foreach { ports =>
          val b     = vi.getBounds
          val tx    = mousePoint.getX - b.getX
          val ty    = mousePoint.getY - b.getY
//          val idxIn = findPort(ports.inlets, tx, ty)
          activePort = /* if (idxIn >= 0) Some(Port.In(idxIn)) else */ {
            val idxOut = findPort(ports.outlets, tx, ty)
            if (idxOut >= 0) Some(Port.Out(idxOut)) else None
          }
          if (ports.active != activePort) {
            ports.active = activePort
//println("SET " + ports.active)
            vi.setValidated(false)  // force repaint
            vis.repaint()
          }
        }
      }

      def updateCursor() {
        disp.setCursor(if (activePort.isDefined) csrPatch else Cursor.getPredefinedCursor(Cursor.HAND_CURSOR))
      }

      override def itemEntered(vi: VisualItem, e: MouseEvent) {
        activeItem = Some(vi)
        processMove(vi, e)
        updateCursor()
      }

      override def itemExited(vi: VisualItem, e: MouseEvent) {
        activeItem  = None
        if (activePort.isDefined) {
          activePort  = None
          getPorts(vi).foreach(_.active = None)
          vis.repaint()
        }
        disp.setCursor(Cursor.getDefaultCursor)
//        processMove(vi, e)
      }

      override def itemMoved(vi: VisualItem, e: MouseEvent) {
        val hadPort = activePort.isDefined
        processMove(vi, e)
        if (activePort.isDefined != hadPort) updateCursor()
      }

//      /**
//       * @see prefuse.controls.Control#itemPressed(prefuse.visual.VisualItem, java.awt.event.MouseEvent)
//       */
//      public void itemPressed(VisualItem item, MouseEvent e) {
//          if (!SwingUtilities.isLeftMouseButton(e)) return;
//          if ( !fixOnMouseOver ) {
//              wasFixed = item.isFixed();
//              resetItem = true;
//              item.setFixed(true);
//              item.getTable().addTableListener(this);
//          }
//          dragged = false;
//          Display d = (Display)e.getComponent();
//          d.getAbsoluteCoordinate(e.getPoint(), down);
//      }
//
//      /**
//       * @see prefuse.controls.Control#itemReleased(prefuse.visual.VisualItem, java.awt.event.MouseEvent)
//       */
//      public void itemReleased(VisualItem item, MouseEvent e) {
//          if (!SwingUtilities.isLeftMouseButton(e)) return;
//          if ( dragged ) {
//              activeItem = null;
//              item.getTable().removeTableListener(this);
//              if ( resetItem ) item.setFixed(wasFixed);
//              dragged = false;
//          }
//      }
//
//      /**
//       * @see prefuse.controls.Control#itemDragged(prefuse.visual.VisualItem, java.awt.event.MouseEvent)
//       */
//      public void itemDragged(VisualItem item, MouseEvent e) {
//          if (!SwingUtilities.isLeftMouseButton(e)) return;
//          dragged = true;
//          Display d = (Display)e.getComponent();
//          d.getAbsoluteCoordinate(e.getPoint(), temp);
//          double dx = temp.getX()-down.getX();
//          double dy = temp.getY()-down.getY();
//          double x = item.getX();
//          double y = item.getY();
//
//          item.setStartX(x);  item.setStartY(y);
//          item.setX(x+dx);    item.setY(y+dy);
//          item.setEndX(x+dx); item.setEndY(y+dy);
//
//          if ( repaint )
//              item.getVisualization().repaint();
//
//          down.setLocation(temp);
//          if ( action != null )
//              d.getVisualization().run(action);
//      }

      override def mouseEntered(e: MouseEvent) { reportMouse(e) }
      override def mouseDragged(e: MouseEvent) { reportMouse(e) }
      override def mouseMoved(  e: MouseEvent) { reportMouse(e) }
    }

    def getData( vi: VisualItem): Option[VisualElement] = Option(vi.get(COL_ELEM ).asInstanceOf[VisualElement])
    def getPorts(vi: VisualItem): Option[VisualPorts  ] = Option(vi.get(COL_PORTS).asInstanceOf[VisualPorts  ])

    def stopEditing() {
      val txt = disp.getTextEditor.getText
      disp.stopEditing()
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
                vis.repaint()
              }

            case vc: VisualConstant =>

          }
        }
      }
    }

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
            stopEditing()
          }
      })
    }

    val rf = new DefaultRendererFactory(BoxRenderer)
    vis.setRendererFactory(rf)

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

    val frame = new Frame {
      title     = "Sound Designer"
      contents  = new BorderPanel {
        add(Component.wrap(disp), BorderPanel.Position.Center)
        add(transport, BorderPanel.Position.South)
      }
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
      getPorts(vi).foreach(_.update(b))
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
      val mp  = DragControl.mousePoint
      vi.setX(mp.getX - 2)
      vi.setY(mp.getY - 2)
      editObject(vi)
      vis.repaint()
    }
  }
}
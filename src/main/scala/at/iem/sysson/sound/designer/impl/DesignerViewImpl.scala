package at.iem.sysson
package sound
package designer
package impl

import prefuse.{Visualization, Display}
import swing.{BorderPanel, Action, Component, Frame}
import javax.swing.{JTextField, JComponent}
import gui.GUI
import java.awt.event.{ActionEvent, ActionListener, KeyEvent}
import java.awt.Rectangle
import prefuse.data.Graph
import prefuse.controls.{ZoomControl, PanControl}
import javax.swing.event.{DocumentEvent, DocumentListener}
import prefuse.render.DefaultRendererFactory
import prefuse.visual.VisualItem
import de.sciss.audiowidgets.Transport
import de.sciss.synth._

private[designer] object DesignerViewImpl {
  def apply(): DesignerView = new View

  private val Collection = UGenSpec.standardUGens

  private final val GROUP_GRAPH   = "graph"
//  private final val GROUP_NODES   = "graph.nodes"
//  private final val GROUP_EDGES   = "graph.edges"
  private final val COL_ELEM      = "element"
  private final val COL_PORTS     = "ports"

  private final class View extends DesignerViewImpl {
//    private var mode: Mode = Mode.Select

    private var editingNode     = Option.empty[VisualItem]
    private var editingOldText  = ""

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
                Collection.get(name) match {
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
        tf.setFont(Style.font)
        tf.setForeground(Style.selectionColor)
        tf.setBackground(Style.boxColor)
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

    private val rf = new DefaultRendererFactory(new BoxRenderer(this))
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
  def getData( vi: VisualItem): Option[VisualElement]
  def getPorts(vi: VisualItem): Option[VisualPorts  ]
}
package at.iem.sysson
package gui
package impl

import de.sciss.desktop.{UndoManager, OptionPane}
import de.sciss.scalainterpreter.{InterpreterPane, Interpreter, CodePane}
import scala.concurrent.{ExecutionContext, Future}
import scala.swing.{ProgressBar, FlowPanel, Component, Action, BorderPanel, Button, Swing}
import Swing._
import de.sciss.lucre.stm
import de.sciss.lucre.event.Sys
import de.sciss.lucre.synth.expr.{Strings, ExprImplicits}
import scala.util.Failure
import scala.util.Success
import de.sciss.swingplus.Implicits._
import de.sciss.syntaxpane.SyntaxDocument
import at.iem.sysson.gui.edit.EditExprVar
import de.sciss.icons.raphael
import de.sciss.swingplus.OverlayPanel
import de.sciss.swingplus.Implicits._
import java.awt.Color
import javax.swing.Icon
import javax.swing.event.{UndoableEditEvent, UndoableEditListener}
import de.sciss.lucre.expr.Expr
import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import de.sciss.model.impl.ModelImpl

object PatchCodeViewImpl {
  /** We use one shared interpreter for all patch code frames. */
  private lazy val interpreter: Future[Interpreter] = {
    val cfg     = Interpreter.Config()
    cfg.imports = Code.SynthGraph.imports
    Interpreter.async(cfg)
  }

  def apply[S <: Sys[S]](entry: Library.Leaf[S], undoManager: UndoManager)
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): PatchCodeView[S] = {
    val name0   = entry.name.value
    val source0 = entry.source.value
    val sourceH = tx.newHandle(entry.source)(Strings.varSerializer)

    val code    = Code.SynthGraph(source0)

    val res = new Impl[S](undoManager, code, sourceH)
    GUI.fromTx(res.guiInit(name0))
    res
  }

  private final class Impl[S <: Sys[S]](val undoManager: UndoManager, code: Code.SynthGraph,
                                        sourceH: stm.Source[S#Tx, Expr.Var[S, String]])
                                       (implicit val cursor: stm.Cursor[S])
    extends ComponentHolder[Component] with PatchCodeView[S] with ModelImpl[PatchCodeView.Update] {

    private var _dirty = false
    def dirty = _dirty
    def dirty_=(value: Boolean): Unit = if (_dirty != value) {
      _dirty = value
      actionApply.enabled = value
      dispatch(PatchCodeView.DirtyChange(value))
    }

    private val codeCfg = {
      val b = CodePane.Config()
      b.text = code.source
      b.build
    }

    import code.{id => codeID}

    private var codePane: CodePane = _
    private var futCompile = Option.empty[Future[Unit]]
    private var actionApply: Action = _

    def isCompiling: Boolean = {
      GUI.requireEDT()
      futCompile.isDefined
    }

    protected def currentText: String = codePane.editor.getText

    def dispose()(implicit tx: S#Tx) = ()

    def undoAction: Action = Action.wrap(codePane.editor.getActionMap.get("undo"))
    def redoAction: Action = Action.wrap(codePane.editor.getActionMap.get("redo"))

    def save(): Unit = {
      GUI.requireEDT()
      val newCode = currentText
      val edit    = cursor.step { implicit tx =>
        val expr = ExprImplicits[S]
        import expr._
        import Strings.{varSerializer, serializer}
        val source  = sourceH()
        EditExprVar("Change Source Code", source, newCode)
      }
      undoManager.add(edit)
      // this doesn't work properly
      // component.setDirty(value = false) // do not erase undo history

      // so let's clear the undo history now...
      codePane.editor.getDocument.asInstanceOf[SyntaxDocument].clearUndos()
    }

    def guiInit(name0: String): Unit = {
      codePane        = CodePane(codeCfg)
      val iPane       = InterpreterPane.wrapAsync(interpreter, codePane)

      val ggProgress = new ProgressBar() {
        preferredSize = {
          val d = preferredSize
          d.width = math.min(32, d.width)
          d
        }

        maximumSize = {
          val d = maximumSize
          d.width = math.min(32, d.width)
          d
        }

        this.clientProps += "JProgressBar.style" -> "circular"
        indeterminate = true

        visible = false
      }

      val ggProgressInvis = Swing.RigidBox(ggProgress.preferredSize)

      val progressPane = new OverlayPanel {
        contents += ggProgress
        contents += ggProgressInvis
      }

      actionApply = Action("Apply")(save())
      actionApply.enabled = false

      var clearGreen = false

      def compileIcon(colr: Option[Color]): Icon =
        raphael.Icon(extent = 20, fill = colr.getOrElse(raphael.TexturePaint(24)),
          shadow = raphael.WhiteShadow)(raphael.Shapes.Hammer)

      lazy val actionCompile = Action("Compile") {
        if (futCompile.isDefined) {
          // ggStatus.text = "busy!"
          return
        }

        ggProgress      .visible = true
        ggProgressInvis .visible = false

        val newCode = Code(codeID, currentText)
        val fut     = newCode.compileBody()
        futCompile  = Some(fut)
        import ExecutionContext.Implicits.global
        fut.onComplete { res =>
          GUI.defer {
            futCompile = None
            ggProgressInvis .visible = true
            ggProgress      .visible = false
            val iconColr = res match {
              case Success(_) =>
                clearGreen = true
                new Color(0x00, 0xC0, 0x00) // "\u2713"
              case Failure(Code.CompilationFailed()) => Color.red
              // "error!"
              case Failure(Code.CodeIncomplete()) => Color.orange
              // "incomplete!"
              case Failure(e) =>
                e.printStackTrace()
                Color.red
            }
            ggCompile.icon = compileIcon(Some(iconColr))
          }
        }
      }

      lazy val doc = codePane.editor.getDocument.asInstanceOf[SyntaxDocument]
      doc.addUndoableEditListener(
        new UndoableEditListener {
          def undoableEditHappened(e: UndoableEditEvent): Unit =
            if (clearGreen) {
              clearGreen = false
              ggCompile.icon = compileIcon(None)
            }
        }
      )

      doc.addPropertyChangeListener(SyntaxDocument.CAN_UNDO, new PropertyChangeListener {
        def propertyChange(e: PropertyChangeEvent): Unit = dirty = doc.canUndo
      })

      lazy val ggApply  : Button = GUI.toolButton(actionApply  , raphael.Shapes.Check , tooltip = "Save text changes")
      lazy val ggCompile: Button = GUI.toolButton(actionCompile, raphael.Shapes.Hammer, tooltip = "Verify that current buffer compiles")

      val panelBottom = new FlowPanel(FlowPanel.Alignment.Trailing)(
        HGlue, ggApply, ggCompile, progressPane) // HStrut(16))

      val iPaneC  = iPane.component
      val iPaneCW = Component.wrap(iPaneC)
      val top     = iPaneC.getComponent(iPaneC.getComponentCount - 1) match {
        case jc: javax.swing.JComponent =>
          jc.add(panelBottom.peer)
          iPaneCW
        case _ => new BorderPanel {
          add(iPaneCW    , BorderPanel.Position.Center)
          add(panelBottom, BorderPanel.Position.South )
        }
      }

      component = top
      iPane.component.requestFocus()
    }
  }
}
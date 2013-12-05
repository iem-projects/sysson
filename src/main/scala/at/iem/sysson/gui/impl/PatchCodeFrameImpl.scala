package at.iem.sysson
package gui
package impl

import de.sciss.desktop.{UndoManager, OptionPane, Window}
import de.sciss.scalainterpreter.{InterpreterPane, Interpreter, CodePane}
import de.sciss.desktop.impl.WindowImpl
import scala.concurrent.{ExecutionContext, Future}
import scala.swing.{ProgressBar, FlowPanel, Component, Action, BorderPanel, Button, Label, Swing}
import Swing._
import de.sciss.lucre.stm
import de.sciss.lucre.event.Sys
import de.sciss.lucre.synth.expr.{Strings, ExprImplicits}
import de.sciss.lucre.stm.Disposable
import scala.util.Failure
import scala.util.Success
import de.sciss.swingplus.Implicits._
import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import de.sciss.syntaxpane.SyntaxDocument
import at.iem.sysson.gui.edit.EditExprVar
import de.sciss.icons.raphael
import de.sciss.swingplus.OverlayPanel
import de.sciss.swingplus.Implicits._
import java.awt.Color
import javax.swing.Icon
import javax.swing.event.{UndoableEditEvent, UndoableEditListener}

object PatchCodeFrameImpl {
  /** We use one shared interpreter for all patch code frames. */
  private lazy val interpreter = {
    val cfg     = Interpreter.Config()
    cfg.imports = Code.SynthGraph.imports
    Interpreter.async(cfg)
  }

  def apply[S <: Sys[S]](entry: Library.Leaf[S], undoManager: UndoManager)
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): Unit = {
    val name0   = entry.name.value
    val source0 = entry.source.value
    val sourceH = tx.newHandle(entry.source)(Strings.varSerializer)

    val _code   = Code.SynthGraph(source0)

    val res = new Impl[S] {
      protected val contextName = _code.contextName
      protected val _cursor   = cursor
      protected val codeID    = _code.id

      protected def save(): Unit = {
        val newCode = currentText
        val edit    = cursor.step { implicit tx =>
          val expr = ExprImplicits[S]
          import expr._
          import Strings.{varSerializer, serializer}
          val source  = sourceH()
          // source()    = newCode
          EditExprVar("Change Source Code", source, newCode)
        }
        undoManager.add(edit)
      }

      protected lazy val codeCfg = {
        val b = CodePane.Config()
        b.text = _code.source
        b.build
      }

      val observer = entry.name.changed.react { implicit tx => ch =>
        GUI.fromTx {
          name = ch.now
        }
      }
    }

    GUI.fromTx(res.guiInit(name0))
  }

  private abstract class Impl[S <: Sys[S]] {
    protected def codeCfg: CodePane.Config

    protected def contextName: String
    protected def _cursor: stm.Cursor[S]
    protected def codeID: Int

    private var codePane: CodePane = _

    private var futCompile = Option.empty[Future[Unit]]
    protected def observer: Disposable[S#Tx]

    private var component: WindowImpl2 = _

    protected final def currentText: String = codePane.editor.getText

    private var _name = ""

    def name = _name
    def name_=(value: String): Unit = {
      _name = value
      component.updateTitle(value)
    }

    //    private val undo = new UndoManagerImpl {
    //      private var _dirty = false
    //      protected def dirty = _dirty
    //      protected def dirty_=(value: Boolean): Unit = if (_dirty != value) {
    //        _dirty = value
    //        component.setDirty(value)
    //      }
    //    }

    private def checkClose(): Unit = {
      if (futCompile.isDefined) {
        // ggStatus.text = "busy!"
        return
      }

      //      val newText = currentText
      //      val dirty   = newText != codeCfg.text && newText.stripMargin != ""
      val dirty = component.getDirty

      if (dirty) {
        val message = "The code has been edited.\nDo you want to save the changes?"
        val opt = OptionPane.confirmation(message = message, optionType = OptionPane.Options.YesNoCancel,
          messageType = OptionPane.Message.Warning)
        opt.title = s"Close Code Editor - $name"
        opt.show(Some(component)) match {
          case OptionPane.Result.No  =>
          case OptionPane.Result.Yes =>
            save()

          case OptionPane.Result.Cancel | OptionPane.Result.Closed =>
            return
        }
      }
      disposeFromGUI()
    }

    protected def save(): Unit

    private def disposeFromGUI(): Unit = {
      _cursor.step { implicit tx =>
        disposeData()
      }
      component.dispose()
    }

    private def disposeData()(implicit tx: S#Tx): Unit = {
      observer.dispose()
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

      val actionApply = Action("Apply")(save())
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

      codePane.editor.getDocument.addUndoableEditListener(
        new UndoableEditListener {
          def undoableEditHappened(e: UndoableEditEvent): Unit = {
            if (clearGreen) {
              clearGreen = false
              ggCompile.icon = compileIcon(None)
            }
          }
        }
      )

      lazy val ggApply  : Button = GUI.toolButton(actionApply  , raphael.Shapes.Check )
      lazy val ggCompile: Button = GUI.toolButton(actionCompile, raphael.Shapes.Hammer)

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

      component   = new WindowImpl2(top, actionApply)

      name = name0
      component.front()
      iPane.component.requestFocus()
    }

    private class WindowImpl2(top: Component, /* panelBottom: Component, */ actionApply: Action) extends WindowImpl {
      frame =>

      def style = Window.Auxiliary

      def handler = SwingApplication.windowHandler

      def updateTitle(name: String): Unit = {
        title = s"$name : $contextName Code"
      }

      // def setDirty(value: Boolean): Unit = dirty = value
      def getDirty = dirty

      contents = top
      //        new BorderPanel {
      //        add(top        , BorderPanel.Position.Center)
      //        add(panelBottom, BorderPanel.Position.South )
      //      }
      closeOperation  = Window.CloseIgnore

      reactions += {
        case Window.Closing(_) =>
          checkClose()
      }

      private val editor      = codePane.editor
      private val doc         = editor.getDocument.asInstanceOf[SyntaxDocument]
      private val editorMap   = editor.getActionMap
      private val actionUndo  = editorMap.get("undo")
      private val actionRedo  = editorMap.get("redo")

      doc.addPropertyChangeListener(SyntaxDocument.CAN_UNDO, new PropertyChangeListener {
        def propertyChange(e: PropertyChangeEvent): Unit = {
          val d               = doc.canUndo
          dirty               = d
          actionApply.enabled = d
        }
      })

      //      doc.addUndoableEditListener(new UndoableEditListener {
      //        def undoableEditHappened(e: UndoableEditEvent): Unit = if (!dirty && e.getEdit.isSignificant) {
      //          dirty = true
      //        }
      //      })
      //      doc.addDocumentListener(new DocumentListener {
      //        def insertUpdate (e: DocumentEvent): Unit = perform()
      //        def removeUpdate (e: DocumentEvent): Unit = perform()
      //        def changedUpdate(e: DocumentEvent): Unit = perform()
      //
      //        private def perform(): Unit = {
      //          ... // if (dirty && doc.)
      //        }
      //      })

      bindMenus(
        "file.close" -> Action(null)(checkClose()),
        "edit.undo"  -> Action.wrap(actionUndo),
        "edit.redo"  -> Action.wrap(actionRedo)
      )

      pack()
      GUI.centerOnScreen(this)
    }
  }
}
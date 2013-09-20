package at.iem.sysson
package gui
package impl

import de.sciss.desktop.{OptionPane, Window}
import de.sciss.scalainterpreter.CodePane
import de.sciss.desktop.impl.WindowImpl
import scala.swing.{FlowPanel, BorderPanel, Swing, Label, Button, Component}
import scala.concurrent.{ExecutionContext, Future}
import Swing._
import scala.util.Failure
import scala.util.Success

object PatchCodeFrameImpl {
  def apply(_name: String, _code: Code)(saveFun: (String, String) => Unit): Unit = {
    new Impl {
      // val document = doc
      protected val name      = _name
      protected val contextName = _code.contextName
      // protected val _cursor   = cursor
      // protected val codeH     = tx.newHandle(elem.entity)(Codes.serializer)
      protected val codeID    = _code.id

      protected def save(): Unit = saveFun(name, currentText)

      protected val codeCfg = {
        val b = CodePane.Config()
        b.text = _code.source
        b.build
      }
      //      protected val intpCfg = {
      //        val b = Interpreter.Config()
      //        b.imports = Seq(
      //          "de.sciss.mellite._",
      //          "de.sciss.synth._",
      //          "Ops._",
      //          "concurrent.duration._",
      //          "gui.InterpreterFrame.Bindings._"
      //        )
      //        b.build
      //      }

      // guiFromTx(guiInit())
      guiInit()
    }
  }

  private abstract class Impl /* extends CodeFrame[S] */ {
    // protected def intpCfg: Interpreter.Config
    protected def codeCfg: CodePane.Config
    protected def name: String
    protected def contextName: String
    //protected def _cursor: stm.Cursor[S]
    //protected def codeH: stm.Source[S#Tx, Expr[S, Code]]
    protected def codeID: Int

    private var codePane: CodePane        = _
    // private var intp    : Interpreter     = _
    // private var intpPane: InterpreterPane = _
    private var futCompile = Option.empty[Future[Unit]]
    private var ggStatus: Label = _

    private var component: Window = _

    protected final def currentText: String = codePane.editor.getText

    private def checkClose(): Unit = {
      if (futCompile.isDefined) {
        ggStatus.text = "busy!"
        return
      }

      val newText = currentText
      if (newText != codeCfg.text && newText.stripMargin != "") {
        val message = "The code has been edited.\nDo you want to save the changes?"
        val opt = OptionPane.confirmation(message = message, optionType = OptionPane.Options.YesNoCancel,
          messageType = OptionPane.Message.Warning)
        opt.title = s"Close Code Editor - $name"
        opt.show(Some(component)) match {
          case OptionPane.Result.No =>
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
      //      _cursor.step { implicit tx =>
      //        disposeData()
      //      }
      component.dispose()
    }
//
//    final def dispose()(implicit tx: S#Tx): Unit = {
//      disposeData()
//      guiFromTx {
//        comp.dispose()
//        // intp.dispose()
//      }
//    }

//    private def disposeData()(implicit tx: S#Tx): Unit = {
//      // observer.dispose()
//    }

    def guiInit(): Unit = {
      codePane  = CodePane(codeCfg)
      // intp      = Interpreter(intpCfg)
      // intpPane  = InterpreterPane.wrap(intp, codePane)

      ggStatus  = new Label(null)

      val ggCompile = Button("Compile") {
        if (futCompile.isDefined) {
          ggStatus.text = "busy!"
          return
        }
        ggStatus.text = "..."
        val newCode = Code(codeID, currentText)
        val fut     = newCode.compileBody()
        futCompile  = Some(fut)
        import ExecutionContext.Implicits.global
        fut.onComplete { res =>
          GUI.defer {
            futCompile = None
            val st = res match {
              case Success(_) => "\u2713"
              case Failure(Code.CompilationFailed()) =>
                "error!"
              case Failure(Code.CodeIncomplete()) =>
                "incomplete!"
              case Failure(e) =>
                e.printStackTrace()
                "error!"
            }
            ggStatus.text = st
          }
        }
      }
      // GUI.round(ggCompile)

      val panelBottom = new FlowPanel(FlowPanel.Alignment.Trailing)(HGlue, ggStatus, ggCompile, HStrut(16))

      component = new WindowImpl {
        frame =>

        def style = Window.Auxiliary

        def handler = SwingApplication.windowHandler

        title           = s"$name : $contextName Code"
        contents        = new BorderPanel {
          add(Component.wrap(codePane.component), BorderPanel.Position.Center)
          add(panelBottom, BorderPanel.Position.South)
        }
        closeOperation  = Window.CloseIgnore

        reactions += {
          case Window.Closing(_) =>
            checkClose()
        }

        pack()
        GUI.centerOnScreen(this)
        front()
      }
    }
  }
}
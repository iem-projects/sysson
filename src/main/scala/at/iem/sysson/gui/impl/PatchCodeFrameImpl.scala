package at.iem.sysson
package gui
package impl

import de.sciss.desktop.{OptionPane, Window}
import de.sciss.scalainterpreter.{InterpreterPane, Interpreter, CodePane}
import de.sciss.desktop.impl.WindowImpl
import scala.swing.{FlowPanel, BorderPanel, Swing, Label, Button, Component}
import scala.concurrent.{ExecutionContext, Future}
import Swing._
import scala.util.Failure
import scala.util.Success
import de.sciss.lucre.stm
import de.sciss.lucre.event.Sys
import de.sciss.lucre.synth.expr.{Strings, ExprImplicits}
import de.sciss.lucre.stm.Disposable

object PatchCodeFrameImpl {
  //  private lazy val intp = {
  //    val intpCfg = Interpreter.Config()
  //    intpCfg.imports = Code.SynthGraph.imports
  //    Interpreter(intpCfg)
  //  }

  private lazy val intp = {
    val intpCfg = Interpreter.Config()
    intpCfg.imports = Code.SynthGraph.imports
    // val paneCfg = InterpreterPane.Config()
    Interpreter.async(intpCfg)
  }

  def apply[S <: Sys[S]](entry: Library.Leaf[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): Unit = {
    val name0   = entry.name.value
    val source0 = entry.source.value
    val sourceH = tx.newHandle(entry.source)(Strings.varSerializer)

    val _code   = Code.SynthGraph(source0)

    val res = new Impl[S] {
      // val document = doc
      protected val contextName = _code.contextName
      protected val _cursor   = cursor
      // protected val codeH     = tx.newHandle(elem.entity)(Codes.serializer)
      protected val codeID    = _code.id

      protected def save(): Unit = {
        val newCode = currentText
        // saveFun(newCode)
        cursor.step { implicit tx =>
          val expr = ExprImplicits[S]
          import expr._
          // name is not editable right now in the patch code frame
          // l.name()    = newName
          sourceH()() = newCode
        }
      }

      // protected val initialText = _code.source
      protected lazy val codeCfg = {
        val b = CodePane.Config()
        b.text = _code.source
        b.build()
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

      val observer = entry.name.changed.react { implicit tx => ch =>
        GUI.fromTx {
          name = ch.now
        }
      }
    }

    GUI.fromTx(res.guiInit(name0))
  }

  private abstract class Impl[S <: Sys[S]] /* extends CodeFrame[S] */ {
    // protected def intpCfg: Interpreter.Config
    protected def codeCfg: CodePane.Config

    // protected def initialText: String

    protected def contextName: String
    protected def _cursor: stm.Cursor[S]
    //protected def codeH: stm.Source[S#Tx, Expr[S, Code]]
    protected def codeID: Int

    private var codePane: CodePane        = _

    // private var intp    : Interpreter     = _
    // private var intpPane: InterpreterPane = _
    private var futCompile = Option.empty[Future[Unit]]
    private var ggStatus: Label = _
    protected def observer: Disposable[S#Tx]

    private var component: WindowImpl2 = _

    protected final def currentText: String = codePane.editor.getText

    private var _name = ""

    def name = _name
    def name_=(value: String): Unit = {
      _name = value
      component.updateTitle(value)
    }

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
//
//    final def dispose()(implicit tx: S#Tx): Unit = {
//      disposeData()
//      guiFromTx {
//        comp.dispose()
//        // intp.dispose()
//      }
//    }

    private def disposeData()(implicit tx: S#Tx): Unit = {
      observer.dispose()
    }

    def guiInit(name0: String): Unit = {
      // codePane   = CodePane(codeCfg)
      codePane      = CodePane(codeCfg)
      val intpPane  = InterpreterPane.wrapAsync(intp, codePane)

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

      component = new WindowImpl2(Component.wrap(intpPane /* codePane */.component), panelBottom)

      name = name0
      component.front()
    }

    private class WindowImpl2(top: Component, panelBottom: Component) extends WindowImpl {
      frame =>

      def style = Window.Auxiliary

      def handler = SwingApplication.windowHandler

      def updateTitle(name: String): Unit = {
        title = s"$name : $contextName Code"
      }

      contents = new BorderPanel {
        add(top        , BorderPanel.Position.Center)
        add(panelBottom, BorderPanel.Position.South )
      }
      closeOperation  = Window.CloseIgnore

      reactions += {
        case Window.Closing(_) =>
          checkClose()
      }

      pack()
      GUI.centerOnScreen(this)
      // front()
    }
  }
}
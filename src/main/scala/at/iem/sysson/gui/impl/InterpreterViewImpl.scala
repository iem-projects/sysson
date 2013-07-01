package at.iem.sysson
package gui
package impl

import de.sciss.scalainterpreter.{InterpreterPane, Interpreter, CodePane}
import java.io.{IOException, FileInputStream, File}
import swing.Component
import de.sciss.desktop.Window
import de.sciss.desktop.impl.WindowImpl

private[gui] object InterpreterViewImpl {
  def apply(): InterpreterView = new Impl

  private final class Impl extends InterpreterView {
    impl =>

    val intp = {
      val codeCfg = CodePane.Config()

      val file = new File( /* new File( "" ).getAbsoluteFile.getParentFile, */ "interpreter.txt" )
      if( file.isFile ) try {
         val fis  = new FileInputStream( file )
         val txt  = try {
            val arr = new Array[ Byte ]( fis.available() )
            fis.read( arr )
            new String( arr, "UTF-8" )
         } finally {
            fis.close()
         }
         codeCfg.text = txt

      } catch {
         case e: IOException => e.printStackTrace()
      }

      val intpCfg = Interpreter.Config()
      intpCfg.imports = Seq(
        "at.iem.sysson._",
        "Implicits._",
        "de.sciss.synth._",
        "ugen._",
        "Ops._",
        "de.sciss.osc.Implicits._",
        "concurrent.duration._",
        "gui.InterpreterView.Bindings._"
      )

//      intpCfg.bindings = Seq( NamedParam( "replSupport", replSupport ))
//         in.bind( "s", classOf[ Server ].getName, ntp )
//         in.bind( "in", classOf[ Interpreter ].getName, in )

//      intpCfg.out = Some( LogWindow.instance.log.writer )

      InterpreterPane(interpreterConfig = intpCfg, codePaneConfig = codeCfg)
    }

    val component = Component.wrap(intp.component)

    val f = new WindowImpl {
      frame =>

      def style   = Window.Auxiliary
      def handler = SwingApplication.windowHandler

      title     = "Interpreter"
      contents  = impl.component
      closeOperation = Window.CloseDispose
      pack()
      GUI.centerOnScreen(this)
      front()
    }
  }
}
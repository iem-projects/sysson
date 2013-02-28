package at.iem.sysson
package gui
package impl

import de.sciss.scalainterpreter.{InterpreterPane, NamedParam, Interpreter, CodePane}
import java.io.{IOException, FileInputStream, File}
import swing.{Component, Frame}
import javax.swing.WindowConstants
import swing.event.WindowClosing

private[gui] object InterpreterViewImpl {
  def apply(): InterpreterView = new Impl

  private final class Impl extends InterpreterView {
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
        "concurrent.duration._"
      )

//      intpCfg.bindings = Seq( NamedParam( "replSupport", replSupport ))
//         in.bind( "s", classOf[ Server ].getName, ntp )
//         in.bind( "in", classOf[ Interpreter ].getName, in )

//      intpCfg.out = Some( LogWindow.instance.log.writer )

      InterpreterPane( interpreterConfig = intpCfg, codePaneConfig = codeCfg )
    }

    val component = Component.wrap(intp.component)

    val f = new Frame {
      frame =>

      title     = "Interpreter"
      contents  = component
      peer.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
      listenTo(this)
      reactions += {
        case WindowClosing(_) =>
           // this will be recognized by the DocumentViewHandler which invokes dispose() on this view subsequently:

          // XXX TODO: wooop - no hook here?
          // intp.dispose()
          MenuFactory.root.destroy(frame)
          frame.dispose()
      }
      menuBar = MenuFactory.root.create(this)
      pack()
      centerOnScreen()
      open()
    }
  }
}
/*
 *  InterpreterViewImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

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
      if (file.isFile) try {
        val fis = new FileInputStream(file)
        val txt = try {
          val arr = new Array[Byte](fis.available())
          fis.read(arr)
          new String(arr, "UTF-8")
        } finally {
          fis.close()
        }
        codeCfg.text = txt

      } catch {
        case e: IOException => e.printStackTrace()
      }

      val intpCfg = Interpreter.Config()
      intpCfg.imports = List(
        "at.iem.sysson._",
        "at.iem.sysson.Implicits._",
        "de.sciss.synth._",
        "de.sciss.synth.ugen._",
        "de.sciss.synth.Ops._",
        "de.sciss.osc.Implicits._",
        // "scala.concurrent.duration._",
        "at.iem.sysson.gui.InterpreterView.Bindings._"
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
/*
 *  InterpreterViewImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2016 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui
package impl

import java.io.{File, FileInputStream, IOException}

import de.sciss.desktop
import de.sciss.desktop.WindowHandler
import de.sciss.scalainterpreter.{CodePane, Interpreter, InterpreterPane}

import scala.swing.Component

private[gui] object InterpreterViewImpl {
  def apply(): InterpreterView = new Impl

  private final class Impl extends InterpreterView {
    impl =>

    val intp: InterpreterPane = {
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
        "de.sciss.file._",
        "de.sciss.osc.Implicits._",
        "de.sciss.synth._",
        "de.sciss.synth.ugen._",
        "de.sciss.synth.Ops._",
        "de.sciss.synth.swing.Implicits._",     // ScalaCollider swing extensions
        "de.sciss.synth.swing.Plotting._",      // ScalaCollider swing app extensions
        // "scala.concurrent.duration._",
        "at.iem.sysson.gui.InterpreterView.Bindings._"
      )

      InterpreterPane(interpreterConfig = intpCfg, codePaneConfig = codeCfg)
    }

    val component: Component = intp.component

    val f = new desktop.impl.WindowImpl {
      frame =>

      // override def style = desktop.Window.Auxiliary
      def handler: WindowHandler = SwingApplication.windowHandler

      title     = "Interpreter"
      contents  = impl.component
      closeOperation = desktop.Window.CloseDispose
      pack()
      desktop.Util.centerOnScreen(this)
      front()
    }
  }
}
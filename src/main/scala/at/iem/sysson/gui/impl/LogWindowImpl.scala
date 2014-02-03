/*
 *  LogWindowImpl.scala
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

import swing.{Component, ScrollPane, Swing}
import de.sciss.scalainterpreter.LogPane
import java.io.OutputStream
import javax.swing.BorderFactory
import at.iem.sysson.gui.GUI
import swing.event.WindowClosing
import de.sciss.desktop

// lazy window - opens as soon as something goes to the console
private[gui] final class LogWindowImpl extends LogWindow with desktop.impl.WindowImpl {
  frame =>

//  peer.getRootPane.putClientProperty("Window.style", "small")

  override def style = desktop.Window.Auxiliary
  def handler = SwingApplication.windowHandler

  val log = {
    val cfg   = LogPane.Config()
    cfg.rows  = 24
    LogPane(cfg)
  }

  private val observer: OutputStream = new OutputStream {
    override def write(b: Array[Byte], off: Int, len: Int): Unit = {
      log.makeDefault()               // detaches this observer
      log.outputStream.write(b, off, len)
      Swing.onEDT(frame.front())     // there we go
    }

    def write(b: Int): Unit = {
      write(Array(b.toByte), 0, 1)
    }
  }

  def observe(): Unit = {
    Console.setOut(observer)
    Console.setErr(observer)
  }

  observe()
  closeOperation = desktop.Window.CloseIgnore
  reactions += {
    case WindowClosing(_) =>
      frame.visible = false
      observe()
  }

  contents = new ScrollPane {
    contents  = Component.wrap(log.component)
    border    = BorderFactory.createEmptyBorder()
  }

  title   = "Log"
  pack()
  import LogWindow._
  GUI.placeWindow(frame, horizontal = horizontalPlacement, vertical = verticalPlacement, padding = placementPadding)
}
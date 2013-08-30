package at.iem.sysson
package gui

import java.awt.{GraphicsEnvironment, EventQueue}
import swing.Swing
import Swing._
import de.sciss.desktop.Window

object GUI {
  def centerOnScreen(w: Window): Unit = placeWindow(w, 0.5f, 0.5f, 0)

  def placeWindow(w: Window, horizontal: Float, vertical: Float, padding: Int): Unit = {
    val ge  = GraphicsEnvironment.getLocalGraphicsEnvironment
    val bs  = ge.getMaximumWindowBounds
    val b   = w.size
    val x   = (horizontal * (bs.width  - padding * 2 - b.width )).toInt + bs.x + padding
    val y   = (vertical   * (bs.height - padding * 2 - b.height)).toInt + bs.y + padding
    w.location = (x, y)
  }

  def requireEDT(): Unit = require(EventQueue.isDispatchThread)

  def defer(thunk: => Unit): Unit =
    if (EventQueue.isDispatchThread) thunk else Swing.onEDT(thunk)
}
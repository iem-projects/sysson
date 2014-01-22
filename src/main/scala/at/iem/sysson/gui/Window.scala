package at.iem.sysson.gui

import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm.Disposable
import de.sciss.desktop

trait Window[S <: Sys[S]] extends Disposable[S#Tx] {
  def window: desktop.Window
  def view: View[S]
}
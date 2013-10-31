package at.iem.sysson
package gui
package impl

import de.sciss.lucre.event.Sys

object LibraryViewImpl {
  def apply[S <: Sys[S]](library: Library[S])(implicit tx: S#Tx): LibraryView[S] = {
    ???
  }
}

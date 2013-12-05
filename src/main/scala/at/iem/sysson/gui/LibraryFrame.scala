package at.iem.sysson
package gui

import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm

import impl.{LibraryFrameImpl => Impl}

object LibraryFrame {
  def apply[S <: Sys[S]](library: Library[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): LibraryFrame[S] =
    Impl(library)
}
trait LibraryFrame[S <: Sys[S]] {
  def view: LibraryView[S]
}

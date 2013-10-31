package at.iem.sysson
package gui

import de.sciss.lucre.event.Sys
import scala.swing.Component
import at.iem.sysson.gui.impl.{LibraryViewImpl => Impl}

object LibraryView {
  def apply[S <: Sys[S]](library: Library[S])(implicit tx: S#Tx): LibraryView[S] = Impl(library)
}
trait LibraryView[S <: Sys[S]] {
  def component: Component
  def library: Library[S]
}

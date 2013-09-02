package at.iem.sysson.gui

import at.iem.sysson.Library
import scala.swing.Component
import impl.{LibraryViewImpl => Impl}

object LibraryView {
  def apply(library: Library): LibraryView = Impl(library)
}
trait LibraryView {
  def component: Component
  def library: Library
}

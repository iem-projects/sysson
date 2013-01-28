package at.iem.sysson
package gui
package impl

import swing.Frame
import java.awt.Dimension

object DocumentViewImpl {
  def apply(doc: Document): DocumentView = new Impl(doc)

  private final class Impl(doc: Document) extends DocumentView {
    val f = new Frame {
      title   = doc.path
      size    = new Dimension(300, 200)
//      centerOnScreen()
      open()
    }
  }
}
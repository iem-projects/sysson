package at.iem.sysson
package gui

import impl.{DocumentViewImpl => Impl}

object DocumentView {
  def apply(doc: Document): DocumentView = Impl(doc)
}
trait DocumentView
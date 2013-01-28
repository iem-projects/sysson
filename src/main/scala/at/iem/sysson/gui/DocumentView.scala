package at.iem.sysson
package gui

import impl.{DocumentViewImpl => Impl}
import swing.Component

object DocumentView {
  def apply(doc: Document): DocumentView = Impl(doc)
}
trait DocumentView {
  def component: Component
}
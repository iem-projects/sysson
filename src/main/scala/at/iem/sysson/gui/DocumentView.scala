package at.iem.sysson
package gui

import swing.Component
import ucar.nc2

object DocumentView {
//  def apply(doc: Document): DocumentView = Impl(doc)
}
trait DocumentView {
  def document: Document
  def component: Component
  def selectedVariable: Option[nc2.Variable]
  def selectedVariable_=(opt: Option[nc2.Variable]): Unit
}
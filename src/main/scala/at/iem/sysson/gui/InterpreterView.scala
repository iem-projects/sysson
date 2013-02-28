package at.iem.sysson
package gui

import swing.Component
import impl.{InterpreterViewImpl => Impl}

object InterpreterView {
  def apply(): InterpreterView = Impl()
}
trait InterpreterView {
  def component: Component
}
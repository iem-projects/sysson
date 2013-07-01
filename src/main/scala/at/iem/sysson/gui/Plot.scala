package at.iem.sysson
package gui

import scala.swing.Component

object Plot {
  def apply(v: VariableSection): Plot = ???
}
trait Plot {
  def component: Component
}
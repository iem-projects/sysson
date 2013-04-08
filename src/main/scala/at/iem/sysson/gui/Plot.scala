package at.iem.sysson
package gui

import at.iem.sysson.impl.VariableLike
import scala.swing.Component

object Plot {
  def apply(v: VariableSection): Plot = ???
}
trait Plot {
  def component: Component
}
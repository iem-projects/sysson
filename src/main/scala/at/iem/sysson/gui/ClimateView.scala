package at.iem.sysson
package gui

import swing.Component
import ucar.nc2
import impl.{ClimateViewImpl => Impl}

object ClimateView {
  def apply(section: VariableSection): ClimateView = Impl(section)

  def currentSection: Option[VariableSection] = Impl.currentSection
}
trait ClimateView {
  def component: Component
}
package at.iem.sysson
package gui

import swing.Component
import ucar.nc2
import impl.{ClimateViewImpl => Impl}

object ClimateView {
  def apply(section: VariableSection, xDim: nc2.Dimension, yDim: nc2.Dimension): ClimateView =
    Impl(section, xDim, yDim)

  def currentSection: Option[VariableSection] = Impl.currentSection
}
trait ClimateView {
  def component: Component
}
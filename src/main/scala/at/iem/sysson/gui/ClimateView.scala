package at.iem.sysson
package gui

import swing.Component
import ucar.nc2
import impl.{ClimateViewImpl => Impl}

object ClimateView {
  def apply(in: nc2.NetcdfFile, section: VariableSection): ClimateView = Impl(in, section)
}
trait ClimateView {
  def component: Component
}
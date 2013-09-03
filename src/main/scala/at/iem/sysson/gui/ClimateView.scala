package at.iem.sysson
package gui

import swing.Component
import ucar.nc2
import impl.{ClimateViewImpl => Impl}

object ClimateView {
  def apply(document: Document, section: VariableSection, xDim: nc2.Dimension, yDim: nc2.Dimension): ClimateView =
    Impl(document, section, xDim, yDim)

  def currentSection: Option[VariableSection] = Impl.currentSection
}
trait ClimateView {
  def component: Component

  def document: Document
  def section : VariableSection

  var patch: Option[Patch]
}
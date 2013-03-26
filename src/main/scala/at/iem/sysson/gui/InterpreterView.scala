package at.iem.sysson
package gui

import swing.Component
import impl.{InterpreterViewImpl => Impl}
import ucar.nc2

object InterpreterView {
  def apply(): InterpreterView = Impl()

  /** The content of this object is imported into the REPL */
  object Bindings {
    private def document =
      DocumentHandler.instance.allDocuments.toList.headOption.getOrElse(sys.error("No document open"))

    def doc: nc2.NetcdfFile = document.data

    def selectedVariable: nc2.Variable =
      DocumentViewHandler.instance.getView(document).getOrElse(sys.error("No document view")).selectedVariable
        .getOrElse(sys.error("No variable selected"))

    def plotSection = ClimateView.currentSection.getOrElse(sys.error("No variable section plotted"))
  }
}
trait InterpreterView {
  def component: Component
}
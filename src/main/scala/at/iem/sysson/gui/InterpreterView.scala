package at.iem.sysson
package gui

import swing.Component
import impl.{InterpreterViewImpl => Impl}
import ucar.nc2

object InterpreterView {
  def apply(): InterpreterView = Impl()

  object Bindings {
    def doc: nc2.NetcdfFile =
      DocumentHandler.instance.allDocuments.toList.headOption.getOrElse(sys.error("No document open")).data
  }
}
trait InterpreterView {
  def component: Component
}
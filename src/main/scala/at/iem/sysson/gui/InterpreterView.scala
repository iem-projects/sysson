package at.iem.sysson
package gui

import swing.Component
import impl.{InterpreterViewImpl => Impl}
import ucar.nc2
import Implicits._

object InterpreterView {
  def apply(): InterpreterView = Impl()

  /** The content of this object is imported into the REPL */
  object Bindings {
    private def document = {
      //      val docs = DocumentHandler.instance.allDocuments.toIndexedSeq
      //      if (docs.isEmpty) sys.error("No document open")
      //      val doc = docs.last
      //      if (docs.size > 1) println(s"WARNING: multiple documents open. Assuming '${doc.file.name}")
      //      doc
      DocumentViewHandler.instance.activeDocument.getOrElse(sys.error("No document open"))
    }

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
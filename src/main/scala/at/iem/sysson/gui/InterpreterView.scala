/*
 *  InterpreterView.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import swing.Component
import impl.{InterpreterViewImpl => Impl}
import ucar.nc2
import de.sciss.lucre.matrix.DataSource

object InterpreterView {
  def apply(): InterpreterView = Impl()

  /** The content of this object is imported into the REPL */
  object Bindings {
    //    private def document: DataSource[_] = {
    //      //      val docs = DocumentHandler.instance.allDocuments.toIndexedSeq
    //      //      if (docs.isEmpty) sys.error("No document open")
    //      //      val doc = docs.last
    //      //      if (docs.size > 1) println(s"WARNING: multiple documents open. Assuming '${doc.file.name}")
    //      //      doc
    //
    //      // DocumentViewHandler.instance.activeDocument.getOrElse(sys.error("No document open"))
    //
    //    }

    // def doc: nc2.NetcdfFile = document.data

    // def selectedVariable: nc2.Variable = ...
//      DocumentViewHandler.instance.getView(document).getOrElse(sys.error("No document view")).selectedVariable
//        .getOrElse(sys.error("No variable selected"))

    def plotSection = ClimateView.currentSection.getOrElse(sys.error("No variable section plotted"))
  }
}
trait InterpreterView {
  def component: Component
}
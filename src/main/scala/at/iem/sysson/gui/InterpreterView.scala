/*
 *  InterpreterView.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2016 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import at.iem.sysson.gui.impl.{InterpreterViewImpl => Impl}

import scala.swing.Component

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

    // def plotSection = ClimateView.currentSection.getOrElse(sys.error("No variable section plotted"))
  }
}
trait InterpreterView {
  def component: Component
}
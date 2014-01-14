/*
 *  InterpreterView.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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

object InterpreterView {
  def apply(): InterpreterView = Impl()

  /** The content of this object is imported into the REPL */
  object Bindings {
    private def document: DataSourceLike = {
      //      val docs = DocumentHandler.instance.allDocuments.toIndexedSeq
      //      if (docs.isEmpty) sys.error("No document open")
      //      val doc = docs.last
      //      if (docs.size > 1) println(s"WARNING: multiple documents open. Assuming '${doc.file.name}")
      //      doc

      // DocumentViewHandler.instance.activeDocument.getOrElse(sys.error("No document open"))
      ???
    }

    // def doc: nc2.NetcdfFile = document.data

    def selectedVariable: nc2.Variable = ???
//      DocumentViewHandler.instance.getView(document).getOrElse(sys.error("No document view")).selectedVariable
//        .getOrElse(sys.error("No variable selected"))

    def plotSection = ClimateView.currentSection.getOrElse(sys.error("No variable section plotted"))
  }
}
trait InterpreterView {
  def component: Component
}
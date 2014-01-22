/*
 *  LibraryView.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import de.sciss.lucre.event.Sys
import at.iem.sysson.gui.impl.{LibraryViewImpl => Impl}
import de.sciss.lucre.stm

object LibraryView {
  def apply[S <: Sys[S]](library: Library[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): LibraryView[S] = Impl(library)
}
trait LibraryView[S <: Sys[S]] extends View.Editable[S] {
  def library(implicit tx: S#Tx): Library[S]
}

/*
 *  LibraryWindow.scala
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
import de.sciss.lucre.stm

import impl.{LibraryWindowImpl => Impl}

object LibraryWindow {
  def apply[S <: Sys[S]](library: Library[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): LibraryWindow[S] =
    Impl(library)
}
trait LibraryWindow[S <: Sys[S]] {
  def view: LibraryView[S]
}

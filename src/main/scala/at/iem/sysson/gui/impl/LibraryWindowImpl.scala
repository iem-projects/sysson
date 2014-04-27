/*
 *  LibraryWindowImpl.scala
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
package impl

import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm
import de.sciss.desktop

object LibraryWindowImpl {
  def apply[S <: Sys[S]](library: Library[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): LibraryWindow[S] = {
    val view  = LibraryView(library)
    val res   = new Impl(view)
    res.init()
    res
  }

  private final class Impl[S <: Sys[S]](val view: LibraryView[S])
    extends WindowImpl[S](title0 = "Library") with LibraryWindow[S] {

    override def placement = (1f, 0.5f, 20)

    // override protected def style = desktop.Window.Auxiliary
  }
}

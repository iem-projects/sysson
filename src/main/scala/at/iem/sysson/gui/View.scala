/*
 *  View.scala
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

package at.iem.sysson.gui

import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.event.Sys
import scala.swing.Component
import de.sciss.desktop.UndoManager
import de.sciss.lucre.stm

object View {
  trait Cursor[S <: Sys[S]] extends View[S] {
    implicit def cursor: stm.Cursor[S]
  }

  trait Workspace[S <: Sys[S]] extends Cursor[S] {
    def workspace: at.iem.sysson.Workspace[S]
    implicit def cursor: stm.Cursor[S] = workspace.cursor
  }

  trait Editable[S <: Sys[S]] extends Cursor[S] {
    def undoManager: UndoManager
  }

  trait File {
    def file: de.sciss.file.File
  }
}
trait View[S <: Sys[S]] extends Disposable[S#Tx] {
  def component: Component
}

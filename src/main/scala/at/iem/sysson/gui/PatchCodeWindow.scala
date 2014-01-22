/*
 *  PatchCodeWindow.scala
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
import impl.{PatchCodeWindowImpl => Impl}
import de.sciss.desktop.UndoManager

object PatchCodeWindow {
  def apply[S <: Sys[S]](entry: Library.Leaf[S], undoManager: UndoManager)
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): PatchCodeWindow[S] = Impl(entry, undoManager)
}
trait PatchCodeWindow[S <: Sys[S]] extends Window[S] {
  def view: PatchCodeView[S]
}
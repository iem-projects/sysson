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
import de.sciss.lucre.swing.Window
import at.iem.sysson.sound.Patch

object PatchCodeWindow {
  def apply[S <: Sys[S]](entry: Library.Leaf[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S], undoManager: UndoManager): PatchCodeWindow[S] =
    Impl(entry)

  def apply[S <: Sys[S]](patch: Patch[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S], undoManager: UndoManager): PatchCodeWindow[S] =
    Impl(patch)
}
trait PatchCodeWindow[S <: Sys[S]] extends Window[S] {
  def view: PatchCodeView[S]
}
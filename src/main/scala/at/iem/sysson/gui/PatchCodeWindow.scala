/*
 *  PatchCodeWindow.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm
import de.sciss.mellite.Workspace
import impl.{PatchCodeWindowImpl => Impl}
import de.sciss.desktop.UndoManager
import de.sciss.lucre.swing.Window
import at.iem.sysson.sound.Patch
import de.sciss.synth.proc.{Proc, Obj}

object PatchCodeWindow {
  //  def apply[S <: Sys[S]](entry: Library.Leaf[S])
  //                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S],
  //                         undoManager: UndoManager): PatchCodeWindow[S] =
  //    Impl(entry)

  def apply[S <: Sys[S]](proc: Obj.T[S, Proc.Elem])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S],
                         undoManager: UndoManager): PatchCodeWindow[S] =
    Impl(proc)
}
trait PatchCodeWindow[S <: Sys[S]] extends Window[S] {
  def view: PatchCodeView[S]
}
/*
 *  WorkspaceView.scala
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

import swing.Component
import impl.{WorkspaceViewImpl => Impl}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm.Disposable
import de.sciss.desktop.UndoManager

object WorkspaceView {
  def apply[S <: Sys[S]](workspace: Workspace[S])(implicit tx: S#Tx): WorkspaceView[S] = Impl(workspace)
}
trait WorkspaceView[S <: Sys[S]] extends Disposable[S#Tx] {
  implicit def workspace: Workspace[S]

  def component: Component

  def undoManager: UndoManager

  def front(): Unit
}
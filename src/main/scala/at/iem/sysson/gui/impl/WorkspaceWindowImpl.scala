/*
 *  WorkspaceWindowImpl.scala
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
package impl

import de.sciss.lucre.event.Sys

object WorkspaceWindowImpl {
  def apply[S <: Sys[S]](workspace: Workspace[S])(implicit tx: S#Tx): WorkspaceWindow[S] = {
    val view  = WorkspaceView(workspace)
    val res   = new Impl(view)
    res.init()
    //    workspace.addDependent(new Disposable[S#Tx] {
    //      def dispose()(implicit tx: S#Tx): Unit = GUI.fromTx(DocumentHandler.instance)
    //    })
    res
  }

  private final class Impl[S <: Sys[S]](val view: WorkspaceView[S]) extends WindowImpl[S] with WorkspaceWindow[S] {
    override def placement = (0f, 0.5f, 8)

    override def dispose()(implicit tx: S#Tx): Unit = {
      super.dispose()(tx)
      view.workspace.dispose()
    }
  }
}

package at.iem.sysson
package gui
package impl

import de.sciss.lucre.event.Sys

object WorkspaceWindowImpl {
  def apply[S <: Sys[S]](workspace: Workspace[S])(implicit tx: S#Tx): WorkspaceWindow[S] = {
    val view  = WorkspaceView(workspace)
    val res   = new Impl(view)
    res.init()
    res
  }

  private final class Impl[S <: Sys[S]](val view: WorkspaceView[S]) extends WindowImpl[S] with WorkspaceWindow[S] {
    override def placement = (0f, 0.5f, 8)
  }
}

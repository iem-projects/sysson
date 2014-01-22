package at.iem.sysson
package gui

import de.sciss.lucre.event.Sys
import impl.{WorkspaceWindowImpl => Impl}

object WorkspaceWindow {
  def apply[S <: Sys[S]](workspace: Workspace[S])(implicit tx: S#Tx): WorkspaceWindow[S] = Impl(workspace)
}
trait WorkspaceWindow[S <: Sys[S]] extends Window[S] {
  def view: WorkspaceView[S]
}
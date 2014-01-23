/*
 *  AuralWorkspaceHandlerImpl.scala
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
package sound
package impl

import de.sciss.lucre.event.Sys
import scala.concurrent.stm.TMap
import de.sciss.lucre.stm.Disposable

object AuralWorkspaceHandlerImpl {
  def apply(): AuralWorkspaceHandler = new Impl

  private final class Impl extends AuralWorkspaceHandler {
    private val map = TMap.empty[Workspace[_], AuralWorkspace[_]]

    private def removeView[S <: Sys[S]](workspace: Workspace[S])(implicit tx: S#Tx): Unit =
      map.remove(workspace)(tx.peer) // .foreach

    def view[S <: Sys[S]](workspace: Workspace[S])(implicit tx: S#Tx): AuralWorkspace[S] =
      map.get(workspace)(tx.peer).asInstanceOf[Option[AuralWorkspace[S]]].getOrElse {
        val view = AuralWorkspaceImpl(workspace)
        workspace.addDependent(new Disposable[S#Tx] {
          def dispose()(implicit tx: S#Tx): Unit = removeView(workspace)
        })
        map.put(workspace, view)(tx.peer)
        view
      }
  }
}
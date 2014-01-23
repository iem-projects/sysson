/*
 *  AuralWorkspaceHandler.scala
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

import impl.{AuralWorkspaceHandlerImpl => Impl}
import de.sciss.lucre.event.Sys

object AuralWorkspaceHandler {
  lazy val instance: AuralWorkspaceHandler = Impl()
}
trait AuralWorkspaceHandler {
  def view[S <: Sys[S]](workspace: Workspace[S])(implicit tx: S#Tx): AuralWorkspace[S]
}
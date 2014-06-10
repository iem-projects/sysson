/*
 *  AuralWorkspaceHandler.scala
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
package sound

import impl.{AuralWorkspaceHandlerImpl => Impl}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.{stm, synth}
import de.sciss.mellite.Workspace

object AuralWorkspaceHandler {
  lazy val instance: AuralWorkspaceHandler = Impl()
}
trait AuralWorkspaceHandler {
  def view[S <: Sys[S], I1 <: synth.Sys[I1]](workspace: Workspace[S] { type I = I1 })
                                            (implicit tx: S#Tx, cursor: stm.Cursor[S]): AuralWorkspace[S, I1]
}
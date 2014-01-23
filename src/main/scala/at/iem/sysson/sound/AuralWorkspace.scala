/*
 *  AuralWorkspace.scala
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

import de.sciss.lucre.event.Sys

trait AuralWorkspace[S <: Sys[S]] {
  def workspace: Workspace[S]

  def view(sonification: Sonification[S])(implicit tx: S#Tx): AuralSonification[S]
}

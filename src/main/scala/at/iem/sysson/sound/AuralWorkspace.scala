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
import de.sciss.synth.proc.Grapheme
import scala.concurrent.Future

trait AuralWorkspace[S <: Sys[S]] {
  aw =>

  val workspace: Workspace[S]

  type I = workspace.I

  def view(sonification: Sonification[S])(implicit tx: S#Tx): AuralSonification[S]

  // import aw.{workspace => w}

  private[sysson] def graphemeCache(section: VariableSection)(implicit tx: S#Tx): (Grapheme.Elem.Audio[I], Future[Unit])
}

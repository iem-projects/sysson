/*
 *  AuralWorkspace.scala
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

import de.sciss.lucre.event.Sys
import de.sciss.synth.proc.{Obj, Grapheme}
import scala.concurrent.Future
import de.sciss.lucre.synth

trait AuralWorkspace[S <: Sys[S], I1 <: synth.Sys[I1]] {

  val workspace: Workspace[S] { type I = I1 }

  def view(sonification: Obj.T[S, Sonification.Elem])(implicit tx: S#Tx): AuralSonification[S]

  private[sysson] def graphemeCache(section: VariableSection)
                                   (implicit tx: S#Tx): (Grapheme.Elem.Audio[I1], Future[Unit])
}

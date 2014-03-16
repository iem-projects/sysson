/*
 *  SonificationView.scala
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

import de.sciss.lucre.event.Sys
import at.iem.sysson.sound.Sonification
import impl.{SonificationViewImpl => Impl}
import de.sciss.lucre.swing.View

object SonificationView {
  def apply[S <: Sys[S]](sonification: Sonification[S])
                        (implicit tx: S#Tx, workspace: Workspace[S]): SonificationView[S] = Impl(sonification)
}
trait SonificationView[S <: Sys[S]] extends ViewHasWorkspace[S] with View.Editable[S] {
  def sonification(implicit tx: S#Tx): Sonification[S]
}
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
import scala.swing.Component
import at.iem.sysson.sound.Sonification
import de.sciss.lucre.stm.Disposable
import impl.{SonificationViewImpl => Impl}
import de.sciss.desktop.UndoManager

object SonificationView {
  def apply[S <: Sys[S]](workspace: Workspace[S], sonification: Sonification[S])
                        (implicit tx: S#Tx): SonificationView[S] = Impl(workspace, sonification)
}
trait SonificationView[S <: Sys[S]] extends View.Workspace[S] with View.Editable {
  def sonification(implicit tx: S#Tx): Sonification[S]
}
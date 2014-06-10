/*
 *  SonificationView.scala
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
package gui

import de.sciss.lucre.event.Sys
import at.iem.sysson.sound.Sonification
import impl.{SonificationViewImpl => Impl}
import de.sciss.lucre.swing.View
import de.sciss.synth.proc.Obj
import de.sciss.model.Model
import de.sciss.mellite.Workspace
import de.sciss.lucre.stm

object SonificationView {
  def apply[S <: Sys[S]](sonification: Obj.T[S, Sonification.Elem])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): SonificationView[S] =
    Impl(sonification)

  sealed trait Update
  case object Resized extends Update
}
trait SonificationView[S <: Sys[S]]
  extends ViewHasWorkspace[S]
  with View.Editable[S]
  with Model[SonificationView.Update] {

  def sonification(implicit tx: S#Tx): Obj.T[S, Sonification.Elem]
}
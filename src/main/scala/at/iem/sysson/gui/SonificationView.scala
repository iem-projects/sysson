/*
 *  SonificationView.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import at.iem.sysson.gui.impl.{SonificationViewImpl => Impl}
import at.iem.sysson.sound.{AuralSonification, Sonification}
import de.sciss.lucre.{event => evt, stm}
import de.sciss.lucre.swing.View
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.Workspace
import de.sciss.mellite.gui.ViewHasWorkspace
import de.sciss.model.Model

import scala.swing.Action

object SonificationView {
  def apply[S <: Sys[S]](sonification: Sonification.Obj[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): SonificationView[S] =
    Impl(sonification)

  sealed trait Update
  case object Resized extends Update
}
trait SonificationView[S <: evt.Sys[S]]
  extends ViewHasWorkspace[S]
  with View.Editable[S]
  with Model[SonificationView.Update] {

  def sonification(implicit tx: S#Tx): Sonification.Obj[S]

  def status: evt.Observable[S#Tx, AuralSonification.Update /* [S] */]

  def actionBounce: Action
}
/*
 *  SonificationView.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2019 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.View
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.lucre.{event => evt}
import de.sciss.mellite.gui.CanBounce
import de.sciss.model.Model
import de.sciss.synth.proc.Universe
import de.sciss.synth.proc.gui.UniverseView

object SonificationView {
  def apply[S <: SSys[S]](sonification: Sonification[S])
                        (implicit tx: S#Tx, universe: Universe[S]): SonificationView[S] =
    Impl(sonification)

  sealed trait Update
  case object Resized extends Update
}
trait SonificationView[S <: Sys[S]]
  extends UniverseView[S]
  with View.Editable[S]
  with Model[SonificationView.Update] with CanBounce {

  def sonification(implicit tx: S#Tx): Sonification[S]

  def status: evt.Observable[S#Tx, AuralSonification.Update /* [S] */]
}
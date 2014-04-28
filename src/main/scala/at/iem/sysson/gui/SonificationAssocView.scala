/*
 *  SonificationAssocView.scala
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

package at.iem.sysson.gui

import de.sciss.lucre.event.Sys
import de.sciss.lucre.swing.View
import impl.{SonificationAssocViewImpl => Impl}
import at.iem.sysson.sound.Sonification
import at.iem.sysson.Workspace
import de.sciss.desktop.UndoManager

object SonificationAssocView {
  def apply[S <: Sys[S]](source: Sonification.Source[S], dimName: String)
                        (implicit tx: S#Tx, workspace: Workspace[S],
                         undoManager: UndoManager): SonificationAssocView[S] = Impl(source, dimName)
}
trait SonificationAssocView [S <: Sys[S]] extends View[S] {
  // override def component: ...
}

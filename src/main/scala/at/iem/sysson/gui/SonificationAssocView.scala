/*
 *  SonificationAssocView.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2016 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson.gui

import at.iem.sysson.gui.impl.{SonificationAssocViewImpl => Impl}
import at.iem.sysson.sound.Sonification
import de.sciss.desktop.UndoManager
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.View
import de.sciss.mellite.Workspace

object SonificationAssocView {
  def apply[S <: Sys[S]](source: Sonification.Source[S], dimName: String)
                        (implicit tx: S#Tx, workspace: Workspace[S], undoManager: UndoManager,
                         cursor: stm.Cursor[S]): SonificationAssocView[S] =
    Impl(source, dimName)
}
trait SonificationAssocView [S <: Sys[S]] extends View[S] {
  // override def component: ...
}

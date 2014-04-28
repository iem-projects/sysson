/*
 *  SonificationWindow.scala
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
import impl.{SonificationWindowImpl => Impl}
import at.iem.sysson.Workspace
import at.iem.sysson.sound.Sonification
import de.sciss.lucre.swing.Window
import de.sciss.synth.proc.Obj

object SonificationWindow {
  def apply[S <: Sys[S]](sonification: Obj.T[S, Sonification.Elem])
                        (implicit tx: S#Tx, workspace: Workspace[S]): SonificationWindow[S] =
    Impl(sonification)
}
trait SonificationWindow[S <: Sys[S]] extends Window[S] {
  def view: SonificationView[S]
}

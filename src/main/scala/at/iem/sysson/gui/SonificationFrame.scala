/*
 *  SonificationFrame.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson.gui

import de.sciss.lucre.synth.Sys
import impl.{SonificationFrameImpl => Impl}
import at.iem.sysson.sound.Sonification
import de.sciss.lucre.swing.Window
import de.sciss.synth.proc.Obj
import de.sciss.mellite.Workspace
import de.sciss.lucre.stm

object SonificationFrame {
  def apply[S <: Sys[S]](sonification: Obj.T[S, Sonification.Elem])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): SonificationFrame[S] =
    Impl(sonification)
}
trait SonificationFrame[S <: Sys[S]] extends Window[S] {
  def view: SonificationView[S]
}

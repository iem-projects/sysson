/*
 *  SonificationFrame.scala
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

import at.iem.sysson.gui.impl.{SonificationFrameImpl => Impl}
import at.iem.sysson.sound.Sonification
import de.sciss.lucre.stm
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.Workspace

object SonificationFrame {
  def apply[S <: Sys[S]](sonification: Sonification[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): SonificationFrame[S] =
    Impl(sonification)
}
trait SonificationFrame[S <: Sys[S]] extends Window[S] {
  def view: SonificationView[S]
}

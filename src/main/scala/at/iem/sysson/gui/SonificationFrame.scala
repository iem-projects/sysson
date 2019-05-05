/*
 *  SonificationFrame.scala
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

package at.iem.sysson.gui

import at.iem.sysson.gui.impl.{SonificationFrameImpl => Impl}
import at.iem.sysson.sound.Sonification
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.Universe

object SonificationFrame {
  def apply[S <: Sys[S]](sonification: Sonification[S])
                        (implicit tx: S#Tx, universe: Universe[S]): SonificationFrame[S] =
    Impl(sonification)
}
trait SonificationFrame[S <: Sys[S]] extends Window[S] {
  def view: SonificationView[S]
}

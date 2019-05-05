/*
 *  PlotFrame.scala
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

import at.iem.sysson.Plot
import at.iem.sysson.gui.impl.{PlotFrameImpl => Impl}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.Window
import de.sciss.synth.proc.Universe

object PlotFrame {
  def apply[S <: Sys[S]](plot: Plot[S])
                        (implicit tx: S#Tx, universe: Universe[S]): PlotFrame[S] =
    Impl(plot)

  def apply[S <: Sys[S]](plot: Plot[S], parent: SonificationView[S])
                        (implicit tx: S#Tx, universe: Universe[S]): PlotFrame[S] =
    Impl(plot, parent)

  def spreadsheet[S <: Sys[S]](plot: Plot[S], parent: Option[SonificationView[S]] = None)
                              (implicit tx: S#Tx, universe: Universe[S]): PlotFrame[S] =
    Impl.spreadsheet(plot, parent)
}
trait PlotFrame[S <: Sys[S]] extends Window[S] {
  def view: PlotView[S]
}

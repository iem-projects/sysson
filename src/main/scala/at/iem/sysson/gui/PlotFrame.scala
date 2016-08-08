/*
 *  PlotFrame.scala
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

import at.iem.sysson.Plot
import at.iem.sysson.gui.impl.{PlotFrameImpl => Impl}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.Window
import de.sciss.mellite.Workspace

object PlotFrame {
  def apply[S <: Sys[S]](plot: Plot[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): PlotFrame[S] =
    Impl(plot)

  def apply[S <: Sys[S]](plot: Plot[S], parent: SonificationView[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): PlotFrame[S] =
    Impl(plot, parent)
}
trait PlotFrame[S <: Sys[S]] extends Window[S] {
  def view: PlotView[S]
}

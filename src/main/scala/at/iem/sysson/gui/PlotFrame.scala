/*
 *  PlotFrame.scala
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

package at.iem.sysson.gui

import at.iem.sysson.Plot
import de.sciss.lucre.event.Sys
import impl.{PlotFrameImpl => Impl}
import de.sciss.lucre.swing.Window
import de.sciss.mellite.Workspace
import de.sciss.lucre.stm

object PlotFrame {
  def apply[S <: Sys[S]](plot: Plot.Obj[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): PlotFrame[S] =
    Impl(plot)
}
trait PlotFrame[S <: Sys[S]] extends Window[S] {
  def view: PlotView[S]
}

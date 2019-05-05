/*
 *  PlotView.scala
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

import at.iem.sysson.gui.impl.{PlotViewImpl => Impl}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.View
import de.sciss.synth.proc.Universe
import de.sciss.synth.proc.gui.UniverseView

object PlotView {
  def apply[S <: Sys[S]](plot: Plot[S], parent: SonificationView[S])
                        (implicit tx: S#Tx, universe: Universe[S]): PlotView[S] =
    Impl(plot, parent)

  def apply[S <: Sys[S]](plot: Plot[S])
                        (implicit tx: S#Tx, universe: Universe[S]): PlotView[S] =
    Impl(plot)

  def spreadsheet[S <: Sys[S]](plot: Plot[S], parent: Option[SonificationView[S]])
                              (implicit tx: S#Tx, universe: Universe[S]): PlotView[S] =
    Impl.mkTableView(plot, parent)
}
trait PlotView[S <: Sys[S]]
  extends UniverseView[S]
  with View.Editable[S] {

  def plot(implicit tx: S#Tx): Plot[S]
}
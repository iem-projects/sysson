/*
 *  PlotStatsView.scala
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

import at.iem.sysson.gui.impl.{PlotStatsViewImpl => Impl}
import at.iem.sysson.{Plot, Stats}
import de.sciss.lucre.stm.Sys
import de.sciss.model.Model
import de.sciss.synth.proc.Universe
import de.sciss.synth.proc.gui.UniverseView

object PlotStatsView {
  def apply[S <: Sys[S]](plot: Plot[S])(implicit tx: S#Tx, universe: Universe[S]): PlotStatsView[S] = Impl(plot)
}
trait PlotStatsView[S <: Sys[S]] extends UniverseView[S] with Model[Stats.Variable] {
  def stats: Option[Stats.Variable]
}
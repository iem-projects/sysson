/*
 *  PlotView.scala
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

package at.iem.sysson
package gui

import at.iem.sysson.gui.impl.{PlotViewImpl => Impl}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.View
import de.sciss.lucre.stm
import de.sciss.mellite.gui.ViewHasWorkspace
import de.sciss.synth.proc.Workspace

object PlotView {
  def apply[S <: Sys[S]](plot: Plot[S], parent: SonificationView[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): PlotView[S] =
    Impl(plot, parent)

  def apply[S <: Sys[S]](plot: Plot[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): PlotView[S] =
    Impl(plot)

  def spreadsheet[S <: Sys[S]](plot: Plot[S], parent: Option[SonificationView[S]])
                              (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): PlotView[S] =
    Impl.mkTableView(plot, parent)
}
trait PlotView[S <: Sys[S]]
  extends ViewHasWorkspace[S]
  with View.Editable[S] {

  def plot(implicit tx: S#Tx): Plot[S]
}
/*
 *  PlotView.scala
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

package at.iem.sysson
package gui

import de.sciss.lucre.{event => evt}
import de.sciss.lucre.stm
import de.sciss.lucre.swing.View
import de.sciss.mellite.Workspace
import de.sciss.mellite.gui.ViewHasWorkspace
import impl.{PlotViewImpl => Impl}

object PlotView {
  def apply[S <: evt.Sys[S]](plot: Plot.Obj[S], parent: SonificationView[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): PlotView[S] =
    Impl(plot, parent)

  def apply[S <: evt.Sys[S]](plot: Plot.Obj[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): PlotView[S] =
    Impl(plot)
}
trait PlotView[S <: evt.Sys[S]]
  extends ViewHasWorkspace[S]
  with View.Editable[S] {

  def plot(implicit tx: S#Tx): Plot.Obj[S]
}
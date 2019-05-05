/*
 *  PlotFrameImpl.scala
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
package impl

import de.sciss.lucre.expr.CellView
import de.sciss.lucre.stm.Sys
import de.sciss.mellite.gui.impl.WindowImpl
import de.sciss.synth.proc.Universe

object PlotFrameImpl {
  def apply[S <: Sys[S]](obj: Plot[S], parent: SonificationView[S])
                        (implicit tx: S#Tx, universe: Universe[S]): PlotFrame[S] = {
    val view = PlotView(obj, parent)
    mk(obj, view)
  }

  def apply[S <: Sys[S]](obj: Plot[S])
                        (implicit tx: S#Tx, universe: Universe[S]): PlotFrame[S] = {
    val view = PlotView(obj)
    mk(obj, view)
  }

  def spreadsheet[S <: Sys[S]](obj: Plot[S], parent: Option[SonificationView[S]])
                              (implicit tx: S#Tx, universe: Universe[S]): PlotFrame[S] = {
    val view = PlotView.spreadsheet(obj, parent)
    mk(obj, view)
  }

  private def mk[S <: Sys[S]](obj: Plot[S], view: PlotView[S])(implicit tx: S#Tx): PlotFrame[S] = {
    val name  = CellView.name(obj)
    val res   = new Impl(view, name)
    res.init()
    res
  }

  private final class Impl[S <: Sys[S]](val view: PlotView[S], name: CellView[S#Tx, String])
    extends WindowImpl[S](name /* .map(n => s"$n : Plot") */)
    with PlotFrame[S]
}
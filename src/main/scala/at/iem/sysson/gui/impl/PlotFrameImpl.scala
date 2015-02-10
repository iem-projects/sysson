/*
 *  PlotFrameImpl.scala
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
package impl

import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.Obj
import de.sciss.mellite.{ExprView, Workspace}
import de.sciss.lucre.stm
import de.sciss.mellite.gui.impl.WindowImpl

object PlotFrameImpl {
  def apply[S <: Sys[S]](obj: Obj.T[S, Plot.Elem])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): PlotFrame[S] = {
    val view  = PlotView(obj)
    val name  = ExprView.name(obj)
    val res   = new Impl(view, name)
    res.init()
    res
  }

  private final class Impl[S <: Sys[S]](val view: PlotView[S], name: ExprView[S#Tx, String])
    extends WindowImpl[S](name /* .map(n => s"$n : Plot") */)
    with PlotFrame[S]
}
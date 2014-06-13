/*
 *  DataSourceFrameImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
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

import de.sciss.lucre.event.Sys
import de.sciss.lucre.matrix.DataSource
import de.sciss.desktop
import de.sciss.mellite.Workspace
import de.sciss.lucre.stm
import de.sciss.mellite.gui.impl.WindowImpl

object DataSourceFrameImpl {
  def apply[S <: Sys[S]](source: DataSource[S])(implicit tx: S#Tx, workspace: Workspace[S],
                                                cursor: stm.Cursor[S]): DataSourceFrame[S] = {
    val view  = DataSourceView(source)
    val res   = new Impl(view)
    res.init()
    res
  }

  private final class Impl[S <: Sys[S]](val view: DataSourceView[S])
    extends WindowImpl[S] with DataSourceFrame[S] {

    // override protected def style = desktop.Window.Auxiliary
  }
}

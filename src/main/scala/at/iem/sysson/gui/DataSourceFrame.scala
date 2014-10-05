/*
 *  DataSourceFrame.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import de.sciss.lucre.event.Sys
import de.sciss.synth.proc.Obj
import at.iem.sysson.gui.impl.{DataSourceFrameImpl => Impl, DataSourceElem}
import de.sciss.lucre.matrix.DataSource
import de.sciss.lucre.swing.Window
import de.sciss.mellite.Workspace
import de.sciss.lucre.stm

object DataSourceFrame {
  def apply[S <: Sys[S]](source: Obj.T[S, DataSourceElem])(implicit tx: S#Tx, workspace: Workspace[S],
                                                cursor: stm.Cursor[S]): DataSourceFrame[S] =
    Impl(source.elem.peer)  // XXX TODO - retain object with attributes
}
trait DataSourceFrame[S <: Sys[S]] extends Window[S] {
  def view: DataSourceView[S]
}

/*
 *  DataSourceWindow.scala
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

import de.sciss.lucre.event.Sys
import impl.{DataSourceWindowImpl => Impl}
import de.sciss.lucre.matrix.DataSource
import de.sciss.lucre.swing.Window
import de.sciss.mellite.Workspace
import de.sciss.lucre.stm

object DataSourceWindow {
  def apply[S <: Sys[S]](source: DataSource[S])(implicit tx: S#Tx, workspace: Workspace[S],
                                                cursor: stm.Cursor[S]): DataSourceWindow[S] =
    Impl(source)
}
trait DataSourceWindow[S <: Sys[S]] extends Window[S] {
  def view: DataSourceView[S]
}

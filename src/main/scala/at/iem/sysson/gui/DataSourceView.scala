/*
 *  DataSourceView.scala
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

import ucar.nc2
import de.sciss.lucre.event.Sys
import impl.{DataSourceViewImpl => Impl}
import de.sciss.lucre.matrix.{Matrix, DataSource}
import de.sciss.lucre.swing.View
import de.sciss.mellite.Workspace
import de.sciss.lucre.stm
import de.sciss.mellite.gui.ViewHasWorkspace

object DataSourceView {
  def apply[S <: Sys[S]](source: DataSource[S])(implicit tx: S#Tx, workspace: Workspace[S],
                                                cursor: stm.Cursor[S]): DataSourceView[S] =
    Impl(source)
}
trait DataSourceView[S <: Sys[S]] extends ViewHasWorkspace[S] with View.File {
  def source(implicit tx: S#Tx): DataSource[S]
  var selectedVariable: Option[nc2.Variable]

  /** Note: Must be called on EDT! */
  def mkSelectedMatrix(): Option[stm.Source[S#Tx, Matrix[S]]]
}
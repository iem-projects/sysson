/*
 *  DataSourceView.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
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
import de.sciss.lucre.matrix.DataSource

object DataSourceView {
  def apply[S <: Sys[S]](source: DataSource[S])(implicit workspace: Workspace[S], tx: S#Tx): DataSourceView[S] =
    Impl(source)
}
trait DataSourceView[S <: Sys[S]] extends View.Workspace[S] with View.File {
  def source(implicit tx: S#Tx): DataSource[S]
  var selectedVariable: Option[nc2.Variable]
}
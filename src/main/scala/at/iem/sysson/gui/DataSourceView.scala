/*
 *  DataSourceView.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import at.iem.sysson.gui.impl.{DataSourceViewImpl => Impl}
import de.sciss.lucre.matrix.{DataSource, Matrix}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.View
import de.sciss.mellite.gui.ViewHasWorkspace
import de.sciss.model.Model
import de.sciss.synth.proc.Workspace
import ucar.nc2

object DataSourceView {
  def apply[S <: Sys[S]](source: DataSource[S])(implicit tx: S#Tx, workspace: Workspace[S],
                                                cursor: stm.Cursor[S]): DataSourceView[S] =
    Impl(source)

  sealed trait Update
  final case class VariableSelection(v: Option[nc2.Variable]) extends Update
}
trait DataSourceView[S <: Sys[S]] extends ViewHasWorkspace[S] with View.File with Model[DataSourceView.Update] {
  def source(implicit tx: S#Tx): DataSource[S]
  var selectedVariable: Option[nc2.Variable]

  /** Note: Must be called on EDT! */
  def mkSelectedMatrix(): Option[stm.Source[S#Tx, Matrix[S]]]
}
/*
 *  ClimateView.scala
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

import de.sciss.lucre.stm
import de.sciss.mellite.gui.ViewHasWorkspace

import ucar.nc2
import impl.{ClimateViewImpl => Impl}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.matrix.DataSource
import de.sciss.mellite.Workspace

object ClimateView {
  def apply[S <: Sys[S]](dataSource: DataSource[S], section: VariableSection, xDim: nc2.Dimension, yDim: nc2.Dimension)
           (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): ClimateView[S] =
    Impl(dataSource, section, xDim, yDim)

  def currentSection: Option[VariableSection] = Impl.currentSection
}
trait ClimateView[S <: Sys[S]] extends ViewHasWorkspace[S] {
  def dataSource(implicit tx: S#Tx): DataSource[S]
  def section : VariableSection
}
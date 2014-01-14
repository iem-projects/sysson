/*
 *  ClimateView.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import swing.Component
import ucar.nc2
import impl.{ClimateViewImpl => Impl}

object ClimateView {
  def apply(document: DataSourceLike, section: VariableSection, xDim: nc2.Dimension, yDim: nc2.Dimension): ClimateView =
    ??? // Impl(document, section, xDim, yDim)

  def currentSection: Option[VariableSection] = Impl.currentSection
}
trait ClimateView {
  def component: Component

  def document: DataSourceLike
  def section : VariableSection

  var patch: Option[PatchOLD]
}
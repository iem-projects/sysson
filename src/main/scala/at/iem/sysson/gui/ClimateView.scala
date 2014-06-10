/*
 *  ClimateView.scala
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

import swing.Component
import ucar.nc2
import impl.{ClimateViewImpl => Impl}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.matrix.DataSource
import de.sciss.synth.SynthGraph
import de.sciss.mellite.Workspace

object ClimateView {
  def apply[S <: Sys[S]](document: DataSource[S], section: VariableSection, xDim: nc2.Dimension, yDim: nc2.Dimension)
           (implicit workspace: Workspace[S], tx: S#Tx): ClimateView[S] =
    Impl(document, section, xDim, yDim)

  def currentSection: Option[VariableSection] = Impl.currentSection
}
trait ClimateView[S <: Sys[S]] {
  def component: Component

  def document(implicit tx: S#Tx): DataSource[S]
  def section : VariableSection

  var patch: Option[SynthGraph]
}
/*
 *  DataSourceFrame.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import at.iem.sysson.gui.impl.{DataSourceFrameImpl => Impl}
import de.sciss.lucre.matrix.DataSource
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.Window
import de.sciss.synth.proc.Universe

object DataSourceFrame {
  def apply[S <: Sys[S]](source: DataSource[S])(implicit tx: S#Tx, universe: Universe[S]): DataSourceFrame[S] =
    Impl(source)
}
trait DataSourceFrame[S <: Sys[S]] extends Window[S] {
  def view: DataSourceView[S]
}

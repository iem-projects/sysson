/*
 *  SonificationSourceView.scala
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

import at.iem.sysson.gui.impl.{SonificationSourceViewImpl => Impl}
import de.sciss.desktop.UndoManager
import de.sciss.lucre.matrix.gui.MatrixView
import de.sciss.lucre.stm.Sys
import de.sciss.synth.proc.Universe
import de.sciss.synth.proc.gui.UniverseView

object SonificationSourceView {
  /** Creates a new view for editing the mapping between a sonification data source matrix and model dimensions.
    *
    * @param key          the key in `map`
    * @param dimKeys      the sonification model/patch dimension keys with which data source matrix dimensions
    *                     will be associated
    */
  def apply[S <: Sys[S]](parent: SonificationView[S],
                         key: String, dimKeys: Vec[String])
                        (implicit tx: S#Tx, universe: Universe[S],
                         undoManager: UndoManager): SonificationSourceView[S] =
    Impl(parent, key, dimKeys)
}
trait SonificationSourceView[S <: Sys[S]] extends UniverseView[S] {
  def matrixView: MatrixView[S]
}
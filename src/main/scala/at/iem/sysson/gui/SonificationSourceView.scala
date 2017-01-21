/*
 *  SonificationSourceView.scala
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

import at.iem.sysson.gui.impl.{SonificationSourceViewImpl => Impl}
import de.sciss.desktop.UndoManager
import de.sciss.lucre.matrix.gui.MatrixView
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.View
import de.sciss.synth.proc.Workspace

object SonificationSourceView {
  /** Creates a new view for editing the mapping between a sonification data source matrix and model dimensions.
    *
    * @param key          the key in `map`
    * @param dimKeys      the sonification model/patch dimension keys with which data source matrix dimensions
    *                     will be associated
    * @param workspace    the workspace within which the source resides
    */
  def apply[S <: Sys[S]](parent: SonificationView[S],
                         key: String, dimKeys: Vec[String])
                        (implicit tx: S#Tx, workspace: Workspace[S],
                         undoManager: UndoManager, cursor: stm.Cursor[S]): SonificationSourceView[S] =
    Impl(parent, key, dimKeys)
}
trait SonificationSourceView[S <: Sys[S]] extends View[S] {
  def matrixView: MatrixView[S]
}
/*
 *  SonificationSourceView.scala
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

package at.iem.sysson.gui

import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm.Disposable
import scala.swing.Component
import at.iem.sysson.gui.impl.{SonificationSourceViewImpl => Impl}
import at.iem.sysson._
import de.sciss.lucre.{stm, expr}
import at.iem.sysson.sound.Sonification
import de.sciss.desktop.UndoManager
import de.sciss.lucre.matrix.gui.MatrixView
import de.sciss.mellite.Workspace

object SonificationSourceView {
  /** Creates a new view for editing the mapping between a sonification data source matrix and model dimensions.
    *
    * @param map          the sonification's mapping between model matrix keys and data source matrices
    * @param key          the key in `map`
    * @param dimKeys      the sonification model/patch dimension keys with which data source matrix dimensions
    *                     will be associated
    * @param workspace    the workspace within which the source resides
    */
  def apply[S <: Sys[S]](map: expr.Map[S, String, Sonification.Source[S], Sonification.Source.Update[S]],
                         key: String, dimKeys: Vec[String])
                        (implicit tx: S#Tx, workspace: Workspace[S],
                         undoManager: UndoManager, cursor: stm.Cursor[S]): SonificationSourceView[S] =
    Impl(map, key, dimKeys)
}
trait SonificationSourceView[S <: Sys[S]] extends Disposable[S#Tx] {
  def component: Component

  def matrixView: MatrixView[S]
}
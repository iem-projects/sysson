/*
 *  SonificationAssocViewImpl.scala
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

package at.iem.sysson.gui
package impl

import at.iem.sysson.sound.Sonification
import de.sciss.desktop.UndoManager
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.synth.proc.Universe

object SonificationAssocViewImpl {
  def apply[S <: Sys[S]](source: Sonification.Source[S], keyName: String)
                        (implicit tx: S#Tx, universe: Universe[S],
                         undoManager: UndoManager): SonificationAssocView[S] = {
    val dims      = source.dims
    val sourceH   = tx.newHandle(source)
    val res = new Impl[S](sourceH, keyName)
    res.init(dims)
    res
  }

  private final class Impl[S <: Sys[S]](protected val sourceH: stm.Source[S#Tx, Sonification.Source[S]],
                                        keyName: String)
                                       (implicit val universe: Universe[S], undo: UndoManager)
    extends DimAssocViewImpl[S](keyName) with SonificationAssocView[S] {

    type Source[S1 <: Sys[S1]] = Sonification.Source[S1]

    protected def flavor: DragAndDrop.Flavor[MappingDrag] = DragAndDrop.SonificationSourceMappingFlavor // IntelliJ highlight bug
  }
}
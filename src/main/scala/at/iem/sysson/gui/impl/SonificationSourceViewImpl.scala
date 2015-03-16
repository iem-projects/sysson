/*
 *  SonificationSourceViewImpl.scala
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
package impl

import java.awt.datatransfer.Transferable
import javax.swing.undo.UndoableEdit

import at.iem.sysson.gui.DragAndDrop.SonificationSourceMappingDrag
import at.iem.sysson.sound.Sonification
import de.sciss.desktop.UndoManager
import de.sciss.lucre.event.Sys
import de.sciss.lucre.matrix.Matrix
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.swing.View
import de.sciss.lucre.swing.edit.EditMutableMap
import de.sciss.lucre.{stm, expr}
import de.sciss.mellite.Workspace
import de.sciss.serial.Serializer

object SonificationSourceViewImpl {
  def apply[S <: Sys[S]](map: expr.Map[S, String, Sonification.Source[S], Sonification.Source.Update[S]],
                         key: String, keyDimNames: Vec[String])
                        (implicit tx: S#Tx, workspace: Workspace[S],
                         undoManager: UndoManager, cursor: stm.Cursor[S]): SonificationSourceView[S] = {
    val res = new Impl[S](key = key, _keys = keyDimNames).init(map)
    res
  }

  private final class Impl[S <: Sys[S]](key: String, _keys: Vec[String])
                                       (implicit workspace: Workspace[S], undoManager: UndoManager, cursor: stm.Cursor[S])
    extends MatrixAssocViewImpl[S](_keys) with SonificationSourceView[S] {
    impl =>

    private var sourceObserver: Disposable[S#Tx] = _
    private var mapHOpt: Option[stm.Source[S#Tx, expr.Map.Modifiable[S, String, Sonification.Source[S], Sonification.Source.Update[S]]]] = _

    type Source[S1 <: Sys[S1]] = Sonification.Source[S1]

    def init(map: expr.Map[S, String, Sonification.Source[S], Sonification.Source.Update[S]])(implicit tx: S#Tx): this.type = {
      mapHOpt = map.modifiableOption.map(tx.newHandle(_))
      sourceObserver = map.changed.react { implicit tx => upd =>
        // println(s"OBSERVE CHANGES $upd")
        upd.changes.foreach {
          case expr.Map.Added  (`key`, source) => updateSource(Some(source)) // .data.file.base))
          case expr.Map.Removed(`key`, source) => updateSource(None)
          case expr.Map.Element(`key`, source, sourceUpdate) =>
          // XXX TODO -- this is not seen yet somehow

          // println(s"MAP.ELEMENT $sourceUpdate")
          // res.update(now       )
          case _ =>
          // println(s"OBSERVED OTHER $other (MY KEY $key")
        }
      }
      init()
      updateSource(map.get(key))
      this
    }

    override def dispose()(implicit tx: S#Tx): Unit = {
      super.dispose()
      sourceObserver.dispose()
    }

    protected def mkAssocView(source: Source[S], key: String)(implicit tx: S#Tx): View[S] =
      SonificationAssocView(source, key)

    protected def sourceSerializer: Serializer[S#Tx, S#Acc, Source[S]] = Sonification.Source.serializer[S]

    protected def canRemoveMatrix: Boolean = mapHOpt.isDefined

    protected def canSetMatrix   : Boolean = mapHOpt.isDefined

    protected def matrix(source: Source[S])(implicit tx: S#Tx): Matrix[S] = source.matrix

    protected def editRemoveMatrix()(implicit tx: S#Tx): Option[UndoableEdit] = mapHOpt.map { mapH =>
      val map = mapH()
      EditMutableMap("Remove Matrix", map, key, None)
    }

    protected def editDropMatrix(m: Matrix[S])(implicit tx: S#Tx): Option[UndoableEdit] = mapHOpt.map { mapH =>
      val map     = mapH()
      val source  = Sonification.Source(m)
      val edit    = EditMutableMap("Assign Matrix", map, key, Some(source))
      edit
    }

    protected def mkDimAssocTransferable(src: stm.Source[S#Tx, Source[S]], key0: String): Transferable =
      DragAndDrop.Transferable(DragAndDrop.SonificationSourceMappingFlavor)(new SonificationSourceMappingDrag {
        type S1 = S
        def source: stm.Source[S#Tx, Sonification.Source[S]] = src
        def key: String = key0
        def workspace: Workspace[S] = impl.workspace
      })
  }
}
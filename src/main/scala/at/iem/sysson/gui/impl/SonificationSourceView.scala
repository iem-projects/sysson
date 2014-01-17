/*
 *  SonificationSourceView.scala
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
package impl

import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm.Disposable
import scala.swing.{Swing, TextField, Component}
import DragAndDrop.DataSourceDrag
import sound.Sonification
import edit.EditMutableMap
import de.sciss.desktop.UndoManager
import de.sciss.lucre.{stm, expr}
import de.sciss.file._

object SonificationSourceView {
  def apply[S <: Sys[S]](workspace: Workspace[S], map: expr.Map[S, String, Sonification.Source[S], Sonification.Source.Update[S]],
                         key: String)
                        (implicit tx: S#Tx, undoManager: UndoManager): SonificationSourceView[S] = {
    val mapHOpt   = map.modifiableOption.map(tx.newHandle(_))
    val dataName0 = map.get(key).map(_.data.file.base)
    val res       = new Impl[S](workspace, mapHOpt, key = key, dataName0 = dataName0)
    res.observer  = map.changed.react {
      implicit tx => upd => upd.changes.foreach {
        case expr.Map.Added  (`key`, source) => res.update(Some(source.data.file.base))
        case expr.Map.Removed(`key`, source) => res.update(None)
        // case expr.Map.Element(`key`, source, sourceUpdate)  => res.update(now       )
        case _ =>
      }
    }

    GUI.fromTx(res.guiInit())
    res
  }

  private final class Impl[S <: Sys[S]](workspace: Workspace[S],
      mapHOpt: Option[stm.Source[S#Tx, expr.Map.Modifiable[S, String, Sonification.Source[S], Sonification.Source.Update[S]]]],
      key: String, dataName0: Option[String])
     (implicit undoManager: UndoManager)
    extends SonificationSourceView[S] with ComponentHolder[Component] {

    import workspace.cursor

    private var ggDataName: TextField = _
    
    var observer: Disposable[S#Tx] = _

    def dispose()(implicit tx: S#Tx): Unit = observer.dispose()
    
    def update(dataName: Option[String])(implicit tx: S#Tx): Unit = {
      GUI.fromTx {
        ggDataName.text = dataName.getOrElse("")
      }
    }

    def guiInit(): Unit = {
      ggDataName  = new TextField(16)
      dataName0.foreach(ggDataName.text = _)
      ggDataName.editable = false
      ggDataName.border   = Swing.CompoundBorder(outside = ggDataName.border,
        inside = IconBorder(Icons.Target(DropButton.IconSize)))
      mapHOpt.foreach { mapH =>
        DropButton.installTransferHandler[DataSourceDrag](ggDataName, DragAndDrop.DataSourceFlavor) { drag0 =>
          if (drag0.workspace == workspace) {
            val drag  = drag0.asInstanceOf[DataSourceDrag { type S1 = S }] // XXX TODO: how to make this more pretty?
            val edit  = cursor.step { implicit tx =>
              // val sources = sonifH().sources
                val map     = mapH()
                val data    = drag.source()
                val source  = Sonification.Source(data)
                EditMutableMap("Assign Data Source", map, key, Some(source))
              }
            undoManager.add(edit)
            true

          } else {
            println("ERROR: Cannot drag data sources across workspaces")
            false
          }
        }
      }
      component = ggDataName
    }
  }
}
trait SonificationSourceView[S <: Sys[S]] extends Disposable[S#Tx] {
  def component: Component
}
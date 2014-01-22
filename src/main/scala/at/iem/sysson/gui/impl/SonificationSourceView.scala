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
import scala.swing.{Orientation, BoxPanel, ComboBox, Label, Swing, TextField, Component}
import at.iem.sysson.gui.DragAndDrop.{DataSourceVarDrag, DataSourceDrag}
import sound.Sonification
import edit.EditMutableMap
import de.sciss.desktop.UndoManager
import de.sciss.lucre.{stm, expr}
import de.sciss.file._
import at.iem.sysson.Implicits._
import javax.swing.GroupLayout
import scalaswingcontrib.group.GroupPanel
import de.sciss.swingplus.Implicits._
import scala.concurrent.stm.{Ref => STMRef}
import language.reflectiveCalls
import de.sciss.lucre.expr.Expr
import de.sciss.model

object SonificationSourceView {
  def apply[S <: Sys[S]](workspace: Workspace[S], map: expr.Map[S, String, Sonification.Source[S], Sonification.Source.Update[S]],
                         key: String, dimKeys: Vec[String])
                        (implicit tx: S#Tx, undoManager: UndoManager): SonificationSourceView[S] = {
    val mapHOpt   = map.modifiableOption.map(tx.newHandle(_))
    val res       = new Impl[S](workspace, mapHOpt, key = key, dimKeys = dimKeys)
    res.observer  = map.changed.react {
      implicit tx => upd => upd.changes.foreach {
        case expr.Map.Added  (`key`, source) => res.updateSource(Some(source)) // .data.file.base))
        case expr.Map.Removed(`key`, source) => res.updateSource(None)
        // case expr.Map.Element(`key`, source, sourceUpdate)  => res.update(now       )
        case _ =>
      }
    }

    GUI.fromTx(res.guiInit())
    res.updateSource(map.get(key))
    res
  }

  private final class Impl[S <: Sys[S]](workspace: Workspace[S],
      mapHOpt: Option[stm.Source[S#Tx, expr.Map.Modifiable[S, String, Sonification.Source[S], Sonification.Source.Update[S]]]],
      key: String, dimKeys: Vec[String])
     (implicit undoManager: UndoManager)
    extends SonificationSourceView[S] with ComponentHolder[Component] {

    import workspace.cursor

    private var ggDataName: TextField = _
    private var ggMappings = Vec.empty[ComboBox[String]]

    var observer: Disposable[S#Tx] = _

    def dispose()(implicit tx: S#Tx): Unit = observer.dispose()

    // private val sourceMap = STMRef(Option.empty[stm.Source[S#Tx, expr.Map[S, String, Expr[S, String], model.Change[String]]]])


    def updateSource(source: Option[Sonification.Source[S]])(implicit tx: S#Tx): Unit = {
      val tup = source.map { source =>
        val v         = source.variable
        val _dataName = v.name // file.base
        val net       = v.data(workspace)
        val _dims     = net.dimensionMap
        val _mapping  = source.dims.iterator.map { case (k, expr) => (k, expr.value) } .toMap
        (_dataName, _dims, _mapping)
      }

      GUI.fromTx {
        tup.fold {
          ggDataName.text = ""
          ggMappings.foreach { gg =>
            gg.peer.setModel(ComboBox.newConstantModel(Seq("")))    // XXX TODO: put some stuff in swingplus
          }
        } { case (dataName, dims, mapping) =>
          ggDataName.text = dataName
          val items = "" :: dims.keys.toList
          (dimKeys zip ggMappings).foreach { case (dimKey, gg) =>
            gg.peer.setModel(ComboBox.newConstantModel(items))
            gg.selection.index = items.indexOf(mapping.getOrElse(dimKey, "")) max 0
          }
        }
      }
    }

    def guiInit(): Unit = {
      ggDataName  = new TextField(16)
      // dataName0.foreach(ggDataName.text = _)
      ggDataName.editable = false
      ggDataName.border   = Swing.CompoundBorder(outside = ggDataName.border,
        inside = IconBorder(Icons.Target(DropButton.IconSize)))
      mapHOpt.foreach { mapH =>
        DropButton.installTransferHandler[DataSourceVarDrag](ggDataName, DragAndDrop.DataSourceVarFlavor) { drag0 =>
          if (drag0.workspace == workspace) {
            val drag  = drag0.asInstanceOf[DataSourceVarDrag { type S1 = S }] // XXX TODO: how to make this more pretty?
            val edit  = cursor.step { implicit tx =>
              // val sources = sonifH().sources
              val map   = mapH()
              val v     = drag.variable()
              val source  = Sonification.Source(v)
              EditMutableMap("Assign Data Source Variable", map, key, Some(source))
            }
            undoManager.add(edit)
            true

          } else {
            println("ERROR: Cannot drag data sources across workspaces")
            false
          }
        }
      }

      val ggMap = dimKeys.map { dimKey =>
        val lb    = new Label(s"$dimKey:")
        val combo = new ComboBox(Seq("")) {
          this.clientProps += "JComboBox.isSquare" -> true
          prototypeDisplayValue = Some("altitude")
        }
        (lb, combo)
      }
      ggMappings = ggMap.map(_._2)

      val pMap = new GroupPanel {
        theHorizontalLayout is Sequential(
          Parallel(ggMap.map(tup => add[GroupLayout#ParallelGroup](tup._1)): _*),
          Parallel(ggMap.map(tup => add[GroupLayout#ParallelGroup](tup._2)): _*)
        )

        theVerticalLayout is Sequential(
          ggMap.map { tup =>
            Parallel(Baseline)(tup._1, tup._2): InGroup[GroupLayout#SequentialGroup]
          }: _*
        )
      }

      component = new BoxPanel(Orientation.Vertical) {
        override lazy val peer = {
          val p = new javax.swing.JPanel with SuperMixin {
            // cf. http://stackoverflow.com/questions/11726739/use-getbaselineint-w-int-h-of-a-child-component-in-the-parent-container
            override def getBaseline(w: Int, h: Int): Int = {
              val size = ggDataName.preferredSize
              ggDataName.location.y + ggDataName.peer.getBaseline(size.width, size.height)
            }
          }
          val l = new javax.swing.BoxLayout(p, Orientation.Vertical.id)
          p.setLayout(l)
          p
        }

        contents += ggDataName
        contents += pMap
      }
    }
  }
}
trait SonificationSourceView[S <: Sys[S]] extends Disposable[S#Tx] {
  def component: Component
}
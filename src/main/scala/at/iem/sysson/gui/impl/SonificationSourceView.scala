/*
 *  SonificationSourceView.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
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
import at.iem.sysson.gui.DragAndDrop.MatrixDrag
import sound.Sonification
import de.sciss.desktop.UndoManager
import de.sciss.lucre.{stm, expr}
import at.iem.sysson.Implicits._
import javax.swing.GroupLayout
import scalaswingcontrib.group.GroupPanel
import de.sciss.swingplus.Implicits._
import language.reflectiveCalls
import scala.swing.event.SelectionChanged
import de.sciss.lucre.expr.Expr
import scala.concurrent.stm.{Ref => STMRef}
import de.sciss.model
import de.sciss.model.Change
import de.sciss.lucre.expr.{String => StringEx}
import de.sciss.lucre.swing.edit.{EditVar, EditMutableMap, EditExprMap}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing._
import de.sciss.lucre.matrix.gui.MatrixView
import de.sciss.lucre.matrix.Matrix

object SonificationSourceView {
  private val DEBUG = true

  def apply[S <: Sys[S]](workspace: Workspace[S], map: expr.Map[S, String, Sonification.Source[S], Sonification.Source.Update[S]],
                         key: String, dimKeys: Vec[String])
                        (implicit tx: S#Tx, undoManager: UndoManager): SonificationSourceView[S] = {
    // XXX TODO: debug
    de.sciss.lucre.event.showLog = true
    de.sciss.lucre.matrix.gui.impl.MatrixViewImpl.DEBUG = true

    import workspace.cursor
    val mapHOpt   = map.modifiableOption.map(tx.newHandle(_))
    val matView   = MatrixView[S]
    val res       = new Impl[S](workspace, matView, mapHOpt, key = key, dimKeys = dimKeys)
    res.sourceObserver  = map.changed.react {
      implicit tx => upd => upd.changes.foreach {
        case expr.Map.Added  (`key`, source) => res.updateSource(Some(source)) // .data.file.base))
        case expr.Map.Removed(`key`, source) => res.updateSource(None)
        // case expr.Map.Element(`key`, source, sourceUpdate)  => res.update(now       )
        case _ =>
      }
    }
    if (DEBUG) println(s"SonificationSourceView(map = $map; ${map.changed}); sourceObserver = ${res.sourceObserver}")

    deferTx(res.guiInit())
    res.updateSource(map.get(key))
    res
  }

  private final class Impl[S <: Sys[S]](workspace: Workspace[S], matrixView: MatrixView[S],
      mapHOpt: Option[stm.Source[S#Tx, expr.Map.Modifiable[S, String, Sonification.Source[S], Sonification.Source.Update[S]]]],
      key: String, dimKeys: Vec[String])
     (implicit undoManager: UndoManager)
    extends SonificationSourceView[S] with ComponentHolder[Component] {

    import workspace.cursor

    private var ggDataName: TextField = _
    private var ggMappings = Vec.empty[ComboBox[String]]

    var sourceObserver: Disposable[S#Tx] = _

    def dispose()(implicit tx: S#Tx): Unit = {
      sourceObserver.dispose()
      matrixView    .dispose()
      disposeDimMap()
    }

    private def disposeDimMap()(implicit tx: S#Tx): Unit = {
      dimMapObserver.swap(None)(tx.peer).foreach { obs =>
        if (DEBUG) println(s"disposeDimMap. observer = $obs")
        obs.dispose()
      }
      dimMap.set(None)(tx.peer)
    }

    private val dimMap = STMRef(
      Option.empty[stm.Source[S#Tx, expr.Map.Modifiable[S, String, Expr[S, String], model.Change[String]]]]
    )
    private val dimMapObserver = STMRef(Option.empty[Disposable[S#Tx]])

    // propagate GUI action back to model
    private def updateDimMapFromGUI(dimKey: String, value: String): Unit = {
      requireEDT()
      val editOpt = cursor.step { implicit tx =>
        dimMap.get(tx.peer).map { mapH =>
          val map = mapH()
          import StringEx.newConst
          implicit val s = StringEx
          val exprOpt: Option[Expr[S, String]] = if (value != "") Some(newConst(value)) else None
          EditExprMap("Map Dimension", map, key = dimKey, value = exprOpt)
        }
      }
      editOpt.foreach(undoManager.add)
    }

    // propagate model change to GUI
    private def updateDimMapToGUI(mapKey: String, value: String)(implicit tx: S#Tx): Unit = {
      val idx = dimKeys.indexOf(mapKey)
      // println(s"updateDimMapToGUI($mapKey, $value)")
      if (idx >= 0) deferTx {
        val gg      = ggMappings(idx)
        // XXX TODO: should have something helpful in swingplus
        val selIdx  = (0 until gg.peer.getItemCount).find(gg.peer.getItemAt(_) == value).getOrElse(-1)
        selectIndex(gg, selIdx)
      }
    }

    private def selectIndex[A](gg: ComboBox[A], index: Int): Unit = {
      requireEDT()
      gg.deafTo  (gg.selection) // otherwise index_= will dispatch event
      gg.selection.index = index max 0
      gg.listenTo(gg.selection)
    }

    def updateSource(sourceOpt: Option[Sonification.Source[S]])(implicit tx: S#Tx): Unit = {
      disposeDimMap()

      matrixView.matrix = sourceOpt.map(_.matrix)

      val tup = sourceOpt.map { source =>
        val v             = source.matrix
        val _dataName     = v.name
        // val net       = v.data()(tx, workspace)
        // val _dims     = net.dimensionMap
        val sDims     = source.dims
        val _mapping  = sDims.iterator.map { case (k, expr) => (k, expr.value) } .toMap

        val mapObs    = sDims.changed.react { implicit tx => upd => upd.changes.foreach {
          case expr.Map.Added  (k, expr)                    => updateDimMapToGUI(k, expr.value)
          case expr.Map.Removed(k, expr)                    => updateDimMapToGUI(k, "")
          case expr.Map.Element(k, expr, Change(_, value))  => updateDimMapToGUI(k, value)
          case _ =>
        }}
        if (DEBUG) println(s"updateSource(sDims = $sDims; ${sDims.changed}). New mapObs = $mapObs")

        import StringEx.serializer
        dimMap.set(sDims.modifiableOption.map(tx.newHandle(_)))(tx.peer)
        dimMapObserver.set(Some(mapObs))(tx.peer)

        (_dataName, /* _dims, */ _mapping)
      }

      deferTx {
        tup.fold {
          ggDataName.text = ""
          ggMappings.foreach { gg =>
            gg.peer.setModel(ComboBox.newConstantModel(Seq("")))    // XXX TODO: put some stuff in swingplus
          }
        } { case (dataName, /* dims, */ mapping) =>
          ggDataName.text = dataName
          val items = "" :: Nil // XXX TODO: dims.keys.toList
          (dimKeys zip ggMappings).foreach { case (dimKey, gg) =>
            gg.peer.setModel(ComboBox.newConstantModel(items))
            selectIndex(gg, items.indexOf(mapping.getOrElse(dimKey, "")))
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
        DropButton.installTransferHandler[MatrixDrag](ggDataName, DragAndDrop.MatrixFlavor) { drag0 =>
          if (drag0.workspace == workspace) {
            val drag  = drag0.asInstanceOf[MatrixDrag { type S1 = S }] // XXX TODO: how to make this more pretty?
            val edit  = cursor.step { implicit tx =>
              val map     = mapH()
              val v       = drag.matrix()
              val name    = "Assign Data Source Variable"
              map.get(key).flatMap(src => Matrix.Var.unapply(src.matrix)).fold {
                val vr      = Matrix.Var(v) // so that the matrix becomes editable in its view
                val source  = Sonification.Source(vr)
                EditMutableMap(name, map, key, Some(source))
              } { vr =>
                EditVar(name, vr, v)
              }
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
          listenTo(selection)
          reactions += {
            case SelectionChanged(_) =>
              // println(s"$dimKey: Selected item ${this.selection.item}")
              updateDimMapFromGUI(dimKey, selection.item)
          }
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

      matrixView.nameVisible = false

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
        contents += matrixView.component
        contents += pMap
      }
    }
  }
}
trait SonificationSourceView[S <: Sys[S]] extends Disposable[S#Tx] {
  def component: Component
}
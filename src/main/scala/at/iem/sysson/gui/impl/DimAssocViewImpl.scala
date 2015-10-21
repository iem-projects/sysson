/*
 *  DimAssocViewImpl.scala
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
package impl

import javax.swing.TransferHandler
import javax.swing.TransferHandler.TransferSupport
import javax.swing.undo.UndoableEdit

import de.sciss.desktop
import de.sciss.desktop.UndoManager
import de.sciss.icons.raphael
import de.sciss.lucre.expr.StringObj
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.edit.EditExprMap
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{View, deferTx}
import de.sciss.lucre.{event => evt, stm}
import de.sciss.mellite.Workspace

import scala.concurrent.stm.Ref
import scala.language.higherKinds
import scala.swing.{Alignment, Component, Label}

/** Building block for dropping dimension keys. E.g. used as column headers in the sonification source view */
abstract class DimAssocViewImpl[S <: Sys[S]](keyName: String)
                                          (implicit workspace: Workspace[S], undo: UndoManager, cursor: stm.Cursor[S])
    extends View[S] with ComponentHolder[Component] { me =>

  // ---- abstract ----

  type Source[S1 <: Sys[S1]]

  protected def sourceH: stm.Source[S#Tx, Source[S]]

  protected def flavor: DragAndDrop.Flavor[MappingDrag]

  // ---- impl ----

  protected type MappingDrag  = DragAndDrop.MappingDrag { type Source[S1 <: Sys[S1]] = me.Source[S1] }
  protected type MappingDragS = MappingDrag { type S1 = S }

  private var bindings: Ref[Set[String]] = _
  private var dimsH: stm.Source[S#Tx, evt.Map[S, String, StringObj]] = _
  private var observer: stm.Disposable[S#Tx] = _
  private var label: Label = _

  /** Sub-classes must call `super` if they override this. */
  def dispose()(implicit tx: S#Tx): Unit = observer.dispose()

  def init(dims: evt.Map[S, String, StringObj])(implicit tx: S#Tx): this.type = {
    dimsH = tx.newHandle(dims)
    val b0: Set[String] = dims.iterator.collect {
      case (key, valueEx) if valueEx.value == keyName => key
    } .toSet
    bindings = Ref(b0)

    observer = dims.changed.react { implicit tx => upd => upd.changes.foreach {
        case evt.Map.Added  (key, valueEx) if valueEx.value == keyName => addBinding   (key)
        case evt.Map.Removed(key, valueEx) if valueEx.value == keyName => removeBinding(key)
// ELEM XXX TODO
//        case evt.Map.Element(key, _, change) =>
//          change match {
//            case Change(_, `keyName`) => addBinding   (key)
//            case Change(`keyName`, _) => removeBinding(key)
//            case _ =>
//          }
        case _ =>
      }
    }

    deferTx(guiInit())
    update(b0.headOption)
    this
  }

  private def addBinding(key: String)(implicit tx: S#Tx): Unit = {
    val b = bindings.transformAndGet(_ + key)(tx.peer)
    update(b.headOption)
  }

  private def removeBinding(key: String)(implicit tx: S#Tx): Unit = {
    val b = bindings.transformAndGet(_ - key)(tx.peer)
    update(b.headOption)
  }

  private def update(key: Option[String])(implicit tx: S#Tx): Unit =
    deferTx {
      label.text = key.orNull
    }

  private def importMapping(drag: MappingDragS)(implicit tx: S#Tx): Option[UndoableEdit] = {
    val thisSource  = sourceH()
    val thatSource  = drag.source()
    if (thisSource != thatSource) None else {
      dimsH().modifiableOption.map { map =>
        implicit val s = StringObj
        // import workspace.cursor
        val exprOpt: Option[StringObj[S]] = Some(StringObj.newConst(keyName))
        EditExprMap[S, String, String, StringObj]("Map Dimension", map, key = drag.key, value = exprOpt)
      }
    }
  }

  private def guiInit(): Unit = {
    type A      = DragAndDrop.MappingDrag
    val lb      = new Label("nnnnn")
    lb.icon = raphael.Icon(extent = 24)(raphael.Shapes.Clip) // .Disconnect)
    desktop.Util.fixSize(lb)
    lb.text = null
    lb.horizontalAlignment = Alignment.Leading
    lb.peer.setTransferHandler(new TransferHandler(null) {
      // how to enforce a drop action: https://weblogs.java.net/blog/shan_man/archive/2006/02/choosing_the_dr.html
      override def canImport(support: TransferSupport): Boolean = component.enabled && {
        // println(support.getDataFlavors.mkString("---supported flavours:---\n ", "\n ", ""))
        // println(s"drop actions: ${support.getSourceDropActions}")

        val res =
          if (support.isDataFlavorSupported(flavor) &&
            ((support.getSourceDropActions & TransferHandler.LINK) != 0)) {
            support.setDropAction(TransferHandler.LINK)
            true
          } else
            false

        // println(s"canImport? $res")
        res
      }

      override def importData(support: TransferSupport): Boolean = {
        val t     = support.getTransferable
        val drag  = t.getTransferData(flavor).asInstanceOf[MappingDragS]
        val editOpt = if (drag.workspace != workspace) None else {
          cursor.step { implicit tx =>
            importMapping(drag)
          }
        }
        editOpt.foreach(undo.add)
        editOpt.isDefined
      }
    })
    label     = lb
    component = mkComponent(lb)
  }

  protected def mkComponent(label: Label): Component = label
}
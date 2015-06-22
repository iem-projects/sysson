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
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.{Expr, String => StringEx}
import de.sciss.lucre.swing.{View, deferTx}
import de.sciss.lucre.swing.edit.EditExprMap
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.{expr, stm}
import de.sciss.mellite.Workspace
import de.sciss.model.Change
import org.scalautils.TypeCheckedTripleEquals

import scala.concurrent.stm.Ref
import scala.language.higherKinds
import scala.swing.{Alignment, Label}

/** Building block for dropping dimension keys. E.g. used as column headers in the sonification source view */
abstract class DimAssocViewImpl[S <: Sys[S]](keyName: String)
                                          (implicit workspace: Workspace[S], undo: UndoManager, cursor: stm.Cursor[S])
    extends View[S] with ComponentHolder[Label] { me =>

  // ---- abstract ----

  type Source[S1 <: Sys[S1]]

  protected def sourceH: stm.Source[S#Tx, Source[S]]

  protected def flavor: DragAndDrop.Flavor[MappingDrag]

  // ---- impl ----

  protected type MappingDrag  = DragAndDrop.MappingDrag { type Source[S1 <: Sys[S1]] = me.Source[S1] }
  protected type MappingDragS = MappingDrag { type S1 = S }

  private var bindings: Ref[Set[String]] = _
  private var dimsH: stm.Source[S#Tx, expr.Map[S, String, Expr[S, String], Change[String]]] = _
  private var observer: stm.Disposable[S#Tx] = _

  /** Sub-classes must call `super` if they override this. */
  def dispose()(implicit tx: S#Tx): Unit = observer.dispose()

  def init(dims: expr.Map[S, String, Expr[S, String], Change[String]])(implicit tx: S#Tx): this.type = {
    import StringEx.{serializer => stringSerializer}
    dimsH = tx.newHandle(dims)

    import TypeCheckedTripleEquals._
    val b0 = dims.iterator.collect {
      case (key, valueEx) if valueEx.value === keyName => key
    } .toSet
    bindings = Ref(b0)

    observer = dims.changed.react { implicit tx => upd => upd.changes.foreach {
        case expr.Map.Added  (key, valueEx) if valueEx.value === keyName => addBinding   (key)
        case expr.Map.Removed(key, valueEx) if valueEx.value === keyName => removeBinding(key)
        case expr.Map.Element(key, _, change) =>
          change match {
            case Change(_, `keyName`) => addBinding   (key)
            case Change(`keyName`, _) => removeBinding(key)
            case _ =>
          }
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
      component.text = key.orNull
    }

  private def importMapping(drag: MappingDragS)(implicit tx: S#Tx): Option[UndoableEdit] = {
    val thisSource  = sourceH()
    val thatSource  = drag.source()
    if (thisSource != thatSource) None else {
      dimsH().modifiableOption.map { map =>
        import StringEx.newConst
        implicit val s = StringEx
        // import workspace.cursor
        val exprOpt: Option[Expr[S, String]] = Some(newConst(keyName))
        EditExprMap("Map Dimension", map, key = drag.key, value = exprOpt)
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
    component = lb
  }
}
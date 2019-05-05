/*
 *  DimAssocViewImpl.scala
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

import de.sciss.desktop
import de.sciss.desktop.UndoManager
import de.sciss.icons.raphael
import de.sciss.lucre.expr.StringObj
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.lucre.swing.edit.EditExprMap
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{View, deferTx}
import de.sciss.lucre.{stm, event => evt}
import de.sciss.model.Change
import de.sciss.synth.proc.Universe
import javax.swing.TransferHandler
import javax.swing.TransferHandler.TransferSupport
import javax.swing.undo.UndoableEdit

import scala.concurrent.stm.{InTxn, Ref, TMap}
import scala.language.higherKinds
import scala.swing.{Alignment, Component, Label}

/** Building block for dropping dimension keys. E.g. used as column headers in the sonification source view */
abstract class DimAssocViewImpl[S <: Sys[S]](keyName: String)
                                            (implicit universe: Universe[S], undo: UndoManager)
    extends View[S] with ComponentHolder[Component] { me =>

  type C = Component

  // ---- abstract ----

  type Source[S1 <: Sys[S1]]

  protected def sourceH: stm.Source[S#Tx, Source[S]]

  protected def flavor: DragAndDrop.Flavor[MappingDrag]

  // ---- impl ----

  protected type MappingDrag  = DragAndDrop.MappingDrag { type Source[S2 <: Sys[S2]] = me.Source[S2] }
  protected type MappingDragS = MappingDrag { type S1 = S }

  private val bindings    = Ref(Vector.empty[String])
  private val bindingObs  = TMap.empty[String, Disposable[S#Tx]]
  private var dimsH: stm.Source[S#Tx, evt.Map[S, String, StringObj]] = _
  private var observer: stm.Disposable[S#Tx] = _
  private var label: Label = _

  /** Sub-classes must call `super` if they override this. */
  def dispose()(implicit tx: S#Tx): Unit = {
    implicit val ptx: InTxn = tx.peer
    observer.dispose()
    bindingObs.foreach(_._2.dispose())
    bindingObs.clear()
  }

  def init(dims: evt.Map[S, String, StringObj])(implicit tx: S#Tx): this.type = {
    dimsH = tx.newHandle(dims)

    observer = dims.changed.react { implicit tx => upd => upd.changes.foreach {
        case evt.Map.Added  (key, valueEx) => testAddBinding(key, valueEx)
        case evt.Map.Removed(key, valueEx) =>
          removeBindingObserver(key)
          if (valueEx.value == keyName) removeBinding(key)
        case _ =>
      }
    }

    deferTx(guiInit())

    dims.iterator.foreach {
      case (key, valueEx) => testAddBinding(key, valueEx)
    }
    this
  }

  private def testAddBinding(key: String, valueEx: StringObj[S])(implicit tx: S#Tx): Unit = {
    addBindingObserver(key, valueEx)
    if (valueEx.value == keyName) addBinding(key)
  }

  private def addBinding(key: String)(implicit tx: S#Tx): Unit = {
    val b = bindings.transformAndGet(key +: _)(tx.peer)
    update(b.headOption)
  }

  private def removeBinding(key: String)(implicit tx: S#Tx): Unit = {
    val b = bindings.transformAndGet(_ diff Vector(key))(tx.peer)
    update(b.headOption)
  }

  private def addBindingObserver(key: String, valueEx: StringObj[S])(implicit tx: S#Tx): Unit = {
    val obs = valueEx.changed.react { implicit tx => {
      case Change(_, `keyName`) => addBinding   (key)
      case Change(`keyName`, _) => removeBinding(key)
      case _ =>
    }}
    implicit val ptx: InTxn = tx.peer
    bindingObs.put(key, obs)
  }

  private def removeBindingObserver(key: String)(implicit tx: S#Tx): Unit = {
    implicit val ptx: InTxn = tx.peer
    bindingObs.remove(key).foreach(_.dispose())
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
        implicit val s: StringObj.type = StringObj
        import universe.cursor
        val exprOpt: Option[StringObj[S]] = Some(StringObj.newConst(keyName))
        EditExprMap[S, String, String, StringObj]("Map Dimension", map, key = drag.key, value = exprOpt)
      }
    }
  }

  private def guiInit(): Unit = {
//    type A      = DragAndDrop.MappingDrag
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
        val editOpt = if (drag.workspace != universe.workspace) None else {
          universe.cursor.step { implicit tx =>
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
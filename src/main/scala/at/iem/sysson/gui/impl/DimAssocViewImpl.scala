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
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.swing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.{expr, stm}
import de.sciss.mellite.Workspace
import de.sciss.model.Change

import scala.concurrent.stm.Ref
import scala.swing.{Alignment, Label}

abstract class DimAssocViewImpl[S <: Sys[S]](keyName: String)
                                          (implicit workspace: Workspace[S], undo: UndoManager, cursor: stm.Cursor[S])
    extends SonificationAssocView[S] with ComponentHolder[Label] {

  protected def observer: Disposable[S#Tx]

  private var bindings: Ref[Set[String]] = _
  
  def init(dims: expr.Map[S, String, Expr[S, String], Change[String]])(implicit tx: S#Tx): Unit = {
    val b0 = dims.iterator.collect {
      case (key, valueEx) if valueEx.value == keyName => key
    } .toSet
    bindings = Ref(b0)

    update(b0.headOption)

    deferTx(guiInit())
  }
  
  final def dispose()(implicit tx: S#Tx): Unit = {
    observer.dispose()
  }

  final protected def addBinding(key: String)(implicit tx: S#Tx): Unit = {
    val b = bindings.transformAndGet(_ + key)(tx.peer)
    update(b.headOption)
  }

  final protected def removeBinding(key: String)(implicit tx: S#Tx): Unit = {
    val b = bindings.transformAndGet(_ - key)(tx.peer)
    update(b.headOption)
  }

  private def update(key: Option[String])(implicit tx: S#Tx): Unit =
    deferTx {
      component.text = key.orNull
    }

  protected def importMapping(drag: DragAndDrop.MappingDrag { type S1 = S })(implicit tx: S#Tx): Option[UndoableEdit]
  
  protected def guiInit(): Unit = {
    val flavor  = DragAndDrop.MappingFlavor
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
        val drag  = t.getTransferData(flavor).asInstanceOf[A]
        val editOpt = if (drag.workspace != workspace) None else {
          val drag1 = drag.asInstanceOf[DragAndDrop.MappingDrag { type S1 = S }]
          cursor.step { implicit tx =>
            importMapping(drag1)
          }
        }
        editOpt.foreach(undo.add)
        editOpt.isDefined
      }
    })
    component = lb
  }
}
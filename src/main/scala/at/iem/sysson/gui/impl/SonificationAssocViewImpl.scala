/*
 *  SonificationAssocViewImpl.scala
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

import de.sciss.desktop
import de.sciss.lucre.event.Sys
import scala.swing.{Alignment, Label}
import de.sciss.icons.raphael
import javax.swing.TransferHandler
import javax.swing.TransferHandler.TransferSupport
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.deferTx
import at.iem.sysson.sound.Sonification
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.{stm, expr}
import scala.concurrent.stm.Ref
import de.sciss.desktop.UndoManager
import de.sciss.lucre.expr.{Expr, String => StringEx}
import de.sciss.lucre.swing.edit.EditExprMap
import de.sciss.model.Change
import de.sciss.mellite.Workspace

object SonificationAssocViewImpl {
  def apply[S <: Sys[S]](source: Sonification.Source[S], valueDimName: String)
                        (implicit tx: S#Tx, workspace: Workspace[S],
                         undoManager: UndoManager, cursor: stm.Cursor[S]): SonificationAssocView[S] = {
    val dims      = source.dims
    val sourceH   = tx.newHandle(source)
    val res = new Impl[S](sourceH, valueDimName) {
      protected val bindings = Ref(dims.iterator.collect {
        case (key, valueEx) if valueEx.value == valueDimName => key
      } .toSet)

      val observer = dims.changed.react { implicit tx => upd => upd.changes.foreach {
          case expr.Map.Added  (key, valueEx) if valueEx.value == valueDimName => addBinding   (key)
          case expr.Map.Removed(key, valueEx) if valueEx.value == valueDimName => removeBinding(key)
          case expr.Map.Element(key, _, change) =>
            change match {
              case Change(_, `valueDimName`) => addBinding   (key)
              case Change(`valueDimName`, _) => removeBinding(key)
              case _ =>
            }
          case _ =>
        }
      }

      deferTx(guiInit())

      update(bindings.get(tx.peer).headOption)
    }
    res
  }

  private abstract class Impl[S <: Sys[S]](sourceH: stm.Source[S#Tx, Sonification.Source[S]], valueDimName: String)
                                          (implicit workspace: Workspace[S], undo: UndoManager, cursor: stm.Cursor[S])
    extends SonificationAssocView[S] with ComponentHolder[Label] {

    protected def observer: Disposable[S#Tx]

    protected def bindings: Ref[Set[String]]

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

    final protected def update(key: Option[String])(implicit tx: S#Tx): Unit =
      deferTx {
        component.text = key.orNull
      }

    final def guiInit(): Unit = {
      //      new Button(new Action(null) {
      //        // icon = Icons.Target(extent = 16)
      //        icon = raphael.Icon(extent = 16)(raphael.Shapes.Clip) // .Disconnect)
      //        def apply(): Unit = {
      //
      //        }
      //      })
      val flavor  = DragAndDrop.MappingFlavor
      type A      = DragAndDrop.MappingDrag
      val lb      = new Label("nnnnn")
      // icon = Icons.Target(extent = 16)
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
              val thisSource  = sourceH()
              val thatSource  = drag1.source()
              if (thisSource != thatSource) None else {
                thisSource.dims.modifiableOption.map { map =>
                  import StringEx.newConst
                  implicit val s = StringEx
                  // import workspace.cursor
                  val exprOpt: Option[Expr[S, String]] = Some(newConst(valueDimName))
                  EditExprMap("Map Dimension", map, key = drag.key, value = exprOpt)
                }
              }
            }
          }
          editOpt.foreach(undo.add)
          editOpt.isDefined
        }
      })
      component = lb
    }
  }
}
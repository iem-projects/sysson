/*
 *  MatrixDnDViewImpl.scala
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
import java.awt.event.{MouseAdapter, MouseEvent}
import javax.swing.TransferHandler.TransferSupport
import javax.swing.undo.UndoableEdit
import javax.swing.{JComponent, TransferHandler}

import at.iem.sysson.gui.DragAndDrop.MatrixDrag
import de.sciss.desktop
import de.sciss.desktop.UndoManager
import de.sciss.lucre.matrix.{Matrix, Sys}
import de.sciss.lucre.stm
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{View, deferTx}
import de.sciss.lucre.swing.edit.EditVar
import de.sciss.mellite.Workspace
import de.sciss.mellite.gui.ViewHasWorkspace
import de.sciss.serial.Serializer
import de.sciss.swingplus.PopupMenu

import scala.concurrent.stm.Ref
import scala.swing.event.MouseButtonEvent
import scala.swing.{Component, Swing, Orientation, BoxPanel, Action, Label, MenuItem, TextField}

abstract class MatrixDnDViewImpl[S <: Sys[S], Source[S1 <: Sys[S1]]](canSetMatrix: Boolean,
                                                                     canRemoveMatrix: Boolean)
                                                                    (implicit cursor: stm.Cursor[S],
                                                                     workspace: Workspace[S],
                                                                     undoManager: UndoManager)
  extends View.Editable[S] with ViewHasWorkspace[S] with ComponentHolder[Component]
{
  impl =>

  // ---- abstract ----

  protected def matrix(source: Source[S])(implicit tx: S#Tx): Matrix[S]

  protected def editDropMatrix(m: Matrix[S])(implicit tx: S#Tx): Option[UndoableEdit]
  protected def editRemoveMatrix()(implicit tx: S#Tx): Option[UndoableEdit]

  implicit protected def sourceSerializer: Serializer[S#Tx, S#Acc, Source[S]]

  // ----

  private var ggDataName: TextField = _
  private val sourceOptRef = Ref(Option.empty[stm.Source[S#Tx, Source[S]]])

  protected def updateSource(sourceOpt: Option[Source[S]])(implicit tx: S#Tx): Unit = {
    sourceOptRef.set(sourceOpt.map(tx.newHandle(_)))(tx.peer)

    val nameOpt = sourceOpt.map { source =>
      val v         = matrix(source)
      val _dataName = v.name
      _dataName
    }

    deferTx {
      ggDataName.text = nameOpt.getOrElse("")
    }
  }

  def init()(implicit tx: S#Tx): this.type = {
    deferTx(guiInit())
    this
  }

  private def guiInit(): Unit = {
    ggDataName = new TextField(12)
    if (canRemoveMatrix) {
      ggDataName.listenTo(ggDataName.mouse.clicks)
      ggDataName.reactions += {
        case e: MouseButtonEvent if e.triggersPopup =>
          new PopupMenu {
            contents += new MenuItem(Action("Remove Matrix") {
              val editOpt = impl.cursor.step { implicit tx => editRemoveMatrix() }
              editOpt.foreach(undoManager.add)
            })
            show(ggDataName, e.point.x, e.point.y)
          }
      }
    }
    // dataName0.foreach(ggDataName.text = _)
    ggDataName.editable = false
    ggDataName.focusable= false

    val lbDataName = new Label(null: String) {
      icon      = Icons.Target(DropButton.IconSize)
      // border    = null
      focusable = false
      // borderPainted = false
      tooltip   = "Drag or Drop Matrix"

      private object Transfer extends TransferHandler(null) {
        override def getSourceActions(c: JComponent): Int = TransferHandler.LINK | TransferHandler.COPY

        override def createTransferable(c: JComponent): Transferable = {
          val opt = impl.cursor.step { implicit tx =>
            val sourceOpt = sourceOptRef.get(tx.peer).map(_.apply())
            sourceOpt.map { source =>
              val m0  = impl.matrix(source)
              val m   = Matrix.Var.unapply(m0).getOrElse(m0)
              val drag = new MatrixDrag {
                type S1       = S
                val workspace = impl.workspace
                val matrix    = tx.newHandle(m)
              }
              DragAndDrop.Transferable(DragAndDrop.MatrixFlavor)(drag)
            }
          }
          // println(s"createTransferable -> ${opt.isDefined}")
          opt.orNull
        }

        // how to enforce a drop action: https://weblogs.java.net/blog/shan_man/archive/2006/02/choosing_the_dr.html
        override def canImport(support: TransferSupport): Boolean = canSetMatrix && {
          val res =
            if (support.isDataFlavorSupported(DragAndDrop.MatrixFlavor) &&
              ((support.getSourceDropActions & (TransferHandler.LINK | TransferHandler.COPY)) != 0)) {
              if (support.getDropAction != TransferHandler.COPY)
                support.setDropAction(TransferHandler.LINK)
              true
            } else
              false

          res
        }

        override def importData(support: TransferSupport): Boolean = {
          val t         = support.getTransferable
          val isCopy    = support.getDropAction == TransferHandler.COPY
          val drag0     = t.getTransferData(DragAndDrop.MatrixFlavor).asInstanceOf[MatrixDrag]
          if (drag0.workspace == workspace) {
            val drag  = drag0.asInstanceOf[MatrixDrag { type S1 = S }] // XXX TODO: how to make this more pretty?
            val editOpt = impl.cursor.step { implicit tx =>
                val v0        = drag.matrix()
                val v         = if (isCopy) Matrix.Var.unapply(v0).fold(v0)(_.apply()).mkCopy() else v0
                val sourceOpt = sourceOptRef.get(tx.peer).map(_.apply())
                val vrOpt     = sourceOpt.flatMap(src => Matrix.Var.unapply(matrix(src)))
                val res = vrOpt.fold {
                  val vr = Matrix.Var(v) // so that the matrix becomes editable in its view
                  editDropMatrix(vr)
                } { vr =>
                  implicit val csr = impl.cursor
                  val _edit = EditVar("Assign Matrix", vr, v)
                  updateSource(sourceOpt)  // XXX TODO - stupid work-around
                  Some(_edit)
                }
                res
              }
            editOpt.foreach(undoManager.add)
            true

          } else {
            println("ERROR: Cannot drag data sources across workspaces")
            false
          }
        }
      }

      peer.setTransferHandler(Transfer)

      private object Mouse extends MouseAdapter {
        private var dndInitX    = 0
        private var dndInitY    = 0
        private var dndPressed  = false
        private var dndStarted  = false

        override def mousePressed(e: MouseEvent): Unit = {
          dndInitX	  = e.getX
          dndInitY    = e.getY
          dndPressed  = true
          dndStarted	= false
        }

        override def mouseReleased(e: MouseEvent): Unit = {
          dndPressed  = false
          dndStarted	= false
        }

        override def mouseDragged(e: MouseEvent): Unit =
          if (dndPressed && !dndStarted && ((math.abs(e.getX - dndInitX) > 5) || (math.abs(e.getY - dndInitY) > 5))) {
            Transfer.exportAsDrag(peer, e, TransferHandler.LINK)
            dndStarted = true
          }
      }

      peer.addMouseListener      (Mouse)
      peer.addMouseMotionListener(Mouse)
    }
    desktop.Util.fixSize(ggDataName)

    component = new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HStrut(2)
      contents += lbDataName
      contents += Swing.HStrut(2)
      contents += ggDataName
      contents += Swing.HGlue
    }
  }
}

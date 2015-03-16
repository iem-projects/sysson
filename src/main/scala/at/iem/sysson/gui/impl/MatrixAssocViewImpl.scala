/*
 *  MatrixAssocViewImpl.scala
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

import java.awt.Color
import java.awt.datatransfer.Transferable
import java.awt.event.{MouseEvent, MouseAdapter}
import javax.swing.{JComponent, TransferHandler}
import javax.swing.TransferHandler.TransferSupport
import javax.swing.undo.UndoableEdit

import at.iem.sysson.gui.DragAndDrop.MatrixDrag
import de.sciss.desktop
import de.sciss.desktop.UndoManager
import de.sciss.icons.raphael
import de.sciss.lucre.event.Sys
import de.sciss.lucre.matrix.Matrix
import de.sciss.lucre.matrix.gui.MatrixView
import de.sciss.lucre.swing.{View, deferTx}
import de.sciss.lucre.swing.edit.EditVar
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.stm
import de.sciss.mellite.Workspace
import de.sciss.serial.Serializer

import scala.concurrent.stm.Ref
import scala.language.higherKinds
import scala.swing.{Button, Label, BoxPanel, Orientation, Dimension, Swing, Action, MenuItem, PopupMenu, TextField, Component}
import scala.swing.event.MouseButtonEvent

abstract class MatrixAssocViewImpl [S <: Sys[S]](keys: Vec[String])
                                                (implicit workspace: Workspace[S], undoManager: UndoManager,
                                                 cursor: stm.Cursor[S])
  extends View[S] with ComponentHolder[Component] {
  impl =>

  // ---- abstract ----

  protected type Source[S1 <: Sys[S1]]

  protected def matrix(source: Source[S])(implicit tx: S#Tx): Matrix[S]

  protected def canSetMatrix: Boolean

  protected def canRemoveMatrix: Boolean

  protected def editRemoveMatrix()(implicit tx: S#Tx): Option[UndoableEdit]

  protected def editDropMatrix(m: Matrix[S])(implicit tx: S#Tx): Option[UndoableEdit]

  protected def mkAssocView(source: Source[S], key: String)(implicit tx: S#Tx): View[S]

  protected def mkDimAssocTransferable(source: stm.Source[S#Tx, Source[S]], key: String): Transferable

  implicit protected def sourceSerializer: Serializer[S#Tx, S#Acc, Source[S]]

  // ---- impl ----

  private var ggDataName: TextField = _

  private val assocViews = Ref(Vec.empty[View[S]])

  private val sourceOptRef = Ref(Option.empty[stm.Source[S#Tx, Source[S]]])

  private var _matrixView: MatrixView[S] = _

  final def matrixView: MatrixView[S] = _matrixView

  def init()(implicit tx: S#Tx): this.type = {
    implicit val resolver = WorkspaceResolver[S]
    import at.iem.sysson.Stats.executionContext
    _matrixView = MatrixView[S]
    _matrixView.nameVisible = false
    deferTx(guiInit())
    this
  }

  def dispose()(implicit tx: S#Tx): Unit = {
    disposeAssocViews()
    matrixView.dispose()
  }

  private def disposeAssocViews()(implicit tx: S#Tx): Unit = {
    val as = assocViews.swap(Vec.empty)(tx.peer)
    if (as.nonEmpty) {
      matrixView.rowHeaders = Vec.empty
      as.foreach(_.dispose())
    }
  }

  protected def updateSource(sourceOpt: Option[Source[S]])(implicit tx: S#Tx): Unit = {
    disposeAssocViews()

    sourceOptRef.set(sourceOpt.map(tx.newHandle(_)))(tx.peer)
    matrixView.matrix = sourceOpt.map(matrix)

    val nameOpt = sourceOpt.map { source =>
      val v         = matrix(source)
      val _dataName = v.name
      val as        = v.dimensions.map { dim => mkAssocView(source, dim.name) }
      assocViews.set(as)(tx.peer)
      matrixView.rowHeaders = as
      _dataName
    }

    deferTx {
      ggDataName.text = nameOpt.getOrElse("")
    }
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
    // val icnBorder       = IconBorder(Icons.Target(DropButton.IconSize))
    // val lbDataName      = new Label(null, Icons.Target(DropButton.IconSize), Alignment.Leading)
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
          println(s"createTransferable -> ${opt.isDefined}")
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
          val drag0     = t.getTransferData(DragAndDrop.MatrixFlavor).asInstanceOf[MatrixDrag]
          if (drag0.workspace == workspace) {
            val drag  = drag0.asInstanceOf[MatrixDrag { type S1 = S }] // XXX TODO: how to make this more pretty?
            val editOpt = impl.cursor.step { implicit tx =>
                val v0        = drag.matrix()
                val v         = if (support.getDropAction == TransferHandler.COPY) v0.mkCopy() else v0
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

    // ggDataName.border   = icnBorder //
    // Swing.CompoundBorder(outside = ggDataName.border, inside = icnBorder)
    desktop.Util.fixSize(ggDataName)

    val keyDimButs = keys.map { key0 =>
      new DragAndDrop.Button {
        text = key0

        protected def export(): Option[Transferable] = sourceOptRef.single.get.map { src =>
          mkDimAssocTransferable(src, key0)
        }

        protected def sourceAction(mod: Int) = TransferHandler.LINK

        protected def sourceActions: Int =
          TransferHandler.LINK | TransferHandler.COPY | TransferHandler.MOVE
      }
    }
    if (keyDimButs.nonEmpty) {
      val d = new Dimension(0, 0)
      keyDimButs.foreach { but =>
        val pd  = but.preferredSize
        d.width = math.max(d.width, pd.width)
        d.height= math.max(d.height, pd.height)
      }
      keyDimButs.foreach { but =>
        but.preferredSize = d
        but.minimumSize   = d
        but.maximumSize   = d
      }
    }

    val ggMap = new BoxPanel(Orientation.Horizontal) {
      contents += new Label(null) {
        icon = raphael.Icon(extent = 24, fill = Color.gray)(raphael.Shapes.Hand) // Clip
      }
      contents ++= keyDimButs
      contents += Swing.HGlue
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

      contents += new BoxPanel(Orientation.Horizontal) {
        contents += Swing.HStrut(2)
        contents += lbDataName
        contents += Swing.HStrut(2)
        contents += ggDataName
        contents += Swing.HGlue
      }
      contents += matrixView.component
      contents += ggMap
    }
  }
}
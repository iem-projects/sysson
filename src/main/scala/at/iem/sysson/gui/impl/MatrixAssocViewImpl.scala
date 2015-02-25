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
import javax.swing.TransferHandler
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
import scala.swing.{Alignment, Label, BoxPanel, Orientation, Dimension, Swing, Action, MenuItem, PopupMenu, TextField, Component}
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
    val lbDataName      = new Label(null, Icons.Target(DropButton.IconSize), Alignment.Leading)
    // ggDataName.border   = icnBorder //
    // Swing.CompoundBorder(outside = ggDataName.border, inside = icnBorder)
    desktop.Util.fixSize(ggDataName)

    if (canSetMatrix) {
      DropButton.installTransferHandler[MatrixDrag](lbDataName, DragAndDrop.MatrixFlavor) { drag0 =>
        if (drag0.workspace == workspace) {
          val drag  = drag0.asInstanceOf[MatrixDrag { type S1 = S }] // XXX TODO: how to make this more pretty?
          val editOpt = cursor.step { implicit tx =>
              val v       = drag.matrix()
              val sourceOpt = sourceOptRef.get(tx.peer).map(_.apply())
              val vrOpt   = sourceOpt.flatMap(src => Matrix.Var.unapply(matrix(src)))
              // println(s"DROP. vrOpt = $vrOpt; map = $map")
              val res = vrOpt.fold {
                val vr = Matrix.Var(v) // so that the matrix becomes editable in its view
                editDropMatrix(vr)
              } { vr =>
                val _edit = EditVar("Assign Matrix", vr, v)
                updateSource(sourceOpt)  // XXX TODO - stupid work-around
                Some(_edit)
              }

              // refreshName.foreach(ggDataName.text = _)

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
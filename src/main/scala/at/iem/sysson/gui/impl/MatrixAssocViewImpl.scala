/*
 *  MatrixAssocViewImpl.scala
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

package at.iem.sysson
package gui
package impl

import java.awt.Color
import java.awt.datatransfer.Transferable

import de.sciss.desktop.UndoManager
import de.sciss.icons.raphael
import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.matrix.gui.MatrixView
import de.sciss.lucre.matrix.{DataSource, Matrix}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Sys, TxnLike}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{View, deferTx}
import de.sciss.serial.Serializer
import de.sciss.synth.proc.{Universe, Workspace}
import javax.swing.TransferHandler.TransferSupport
import javax.swing.undo.UndoableEdit
import javax.swing.{JPanel, TransferHandler}

import scala.concurrent.stm.{Ref, TxnExecutor}
import scala.language.higherKinds
import scala.swing.{BoxPanel, Component, Dimension, Label, Orientation, Swing}

object MatrixAssocViewImpl {
  private trait IntDrag {
    type S1 <: Sys[S1]
    def workspace: Workspace[S1]
    def source: stm.Source[S1#Tx, IntObj[S1]]
    def value: Int
  }
  private val IntFlavor = DragAndDrop.internalFlavor[IntDrag]
}
abstract class MatrixAssocViewImpl[S <: Sys[S]](keys: Vec[String])
                                               (implicit universe: Universe[S], undoManager: UndoManager)
  extends View[S] with ComponentHolder[Component] {
  impl =>

  import MatrixAssocViewImpl.{IntDrag, IntFlavor}

  type C = Component

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

  private val assocViews = Ref(Vec.empty[View[S]])

  private var _matrixView: MatrixView[S] = _

  final def matrixView: MatrixView[S] = _matrixView

  private object ButtonImpl extends MatrixDnDViewImpl[S, Source](canSetMatrix = canSetMatrix, canRemoveMatrix = canRemoveMatrix) {
    protected def matrix(source: Source[S])(implicit tx: S#Tx): Matrix[S] = impl.matrix(source)
    protected def editRemoveMatrix()(implicit tx: S#Tx): Option[UndoableEdit] = impl.editRemoveMatrix()
    protected def sourceSerializer: Serializer[S#Tx, S#Acc, Source[S]] = impl.sourceSerializer
    protected def editDropMatrix(m: Matrix[S])(implicit tx: S#Tx): Option[UndoableEdit] = impl.editDropMatrix(m)

    override def updateSource(sourceOpt: Option[Source[S]])(implicit tx: S#Tx): Unit = {
      disposeAssocViews()
      super.updateSource(sourceOpt)
      matrixView.matrix = sourceOpt.map(matrix)
      sourceOpt.foreach { source =>
        val v = matrix(source)
        val as = v.dimensions.map { dim => mkAssocView(source, dim.name) }
        assocViews.set(as)(tx.peer)
        matrixView.rowHeaders = as
      }
    }
  }

  protected def updateSource(sourceOpt: Option[Source[S]])(implicit tx: S#Tx): Unit =
    ButtonImpl.updateSource(sourceOpt)

  private object TH extends MatrixView.TransferHandler[S] {
    def canImportInt(t: TransferSupport): Boolean = t.isDataFlavorSupported(IntFlavor)

    def exportInt(x: IntObj[S])(implicit tx: S#Tx): Option[Transferable] = {
      val drag: IntDrag = new IntDrag {
        type S1 = S
        val workspace : Workspace[S1]                 = impl.universe.workspace
        val source    : stm.Source[S1#Tx, IntObj[S1]] = tx.newHandle(x)
        val value     : Int                           = x.value
      }
      val t = DragAndDrop.Transferable(IntFlavor)(drag)
      Some(t)
    }

    def importInt(t: TransferSupport)(implicit tx: S#Tx): Option[IntObj[S]] = {
      val drag = t.getTransferable.getTransferData(IntFlavor).asInstanceOf[IntDrag]
      val x: IntObj[S] = if (drag.workspace == /* === */ impl.universe.workspace) {
        drag.asInstanceOf[IntDrag { type S1 = S }].source()
      } else {
        IntObj.newConst[S](drag.value)
      }
      Some(x)
    }
  }

  def init()(implicit tx: S#Tx): this.type = {
    import universe.workspace
    implicit val resolver: DataSource.Resolver[S] = WorkspaceResolver[S]
    import Stats.executionContext
    import universe.{cursor, genContext}

    _matrixView = MatrixView[S](Some(TH))
    _matrixView.nameVisible = false
    ButtonImpl.init()
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

  private def guiInit(): Unit = {
    val keyDimButs = keys.map { key0 =>
      new DragAndDrop.Button {
        text = key0

        protected def export(): Option[Transferable] =
          TxnExecutor.defaultAtomic.apply { implicit itx =>
            implicit val tx: TxnLike = TxnLike.wrap(itx)
            ButtonImpl.sourceOpt.map { src =>
              mkDimAssocTransferable(src, key0)
            }
          }

        protected def sourceAction(mod: Int): Int = TransferHandler.LINK

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
      override lazy val peer: JPanel with SuperMixin = {
        val p = new JPanel with SuperMixin {
          // cf. http://stackoverflow.com/questions/11726739/use-getbaselineint-w-int-h-of-a-child-component-in-the-parent-container
          override def getBaseline(w: Int, h: Int): Int = {
            val gg   = ButtonImpl.component
            val size = gg.preferredSize
            gg.location.y + gg.peer.getBaseline(size.width, size.height)
          }
        }
        val l = new javax.swing.BoxLayout(p, Orientation.Vertical.id)
        p.setLayout(l)
        p
      }

      contents += mkTopComponent(ButtonImpl.component)
      contents += matrixView.component
      contents += ggMap
    }
  }

  protected def mkTopComponent(c: Component): Component = c
}
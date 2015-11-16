/*
 *  PlotViewImpl.scala
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
import javax.swing.undo.UndoableEdit

import at.iem.sysson.gui.DragAndDrop.PlotMappingDrag
import at.iem.sysson.sound.AuralSonification
import de.sciss.desktop.UndoManager
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.icons.raphael
import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.matrix.{Dimension, Matrix, Reduce}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.lucre.swing.edit.EditVar
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{View, deferTx}
import de.sciss.mellite.Workspace
import de.sciss.mellite.gui.GUI
import de.sciss.serial.Serializer
import org.scalautils.TypeCheckedTripleEquals

import scala.annotation.tailrec
import scala.concurrent.stm.Ref
import scala.concurrent.{Future, Promise}
import scala.swing.event.ButtonClicked
import scala.swing.{BoxPanel, Component, FlowPanel, Orientation, SplitPane, ToggleButton}
import scala.util.{Failure, Success}

object PlotViewImpl {
  private val DEBUG = false

  def apply[S <: Sys[S]](plot: Plot[S], parent: SonificationView[S])(implicit tx: S#Tx, workspace: Workspace[S],
                                            cursor: stm.Cursor[S]): PlotView[S] = mk(plot, Some(parent))

  def apply[S <: Sys[S]](plot: Plot[S])(implicit tx: S#Tx, workspace: Workspace[S],
                                            cursor: stm.Cursor[S]): PlotView[S] = mk(plot, None)

  private def mk[S <: Sys[S]](plot: Plot[S], parentOpt: Option[SonificationView[S]])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): PlotView[S] = {
    implicit val undoMgr    = new UndoManagerImpl
    val isEditable          = Matrix.Var.unapply(plot.matrix).isDefined
    val plotMatrixView      = new PlotMatrixView(canSetMatrix = isEditable, parentOpt = parentOpt).init(plot)
    val statsView           = PlotStatsView(plot)
    val chartView           = PlotChartImpl(plot, statsView)
    val res                 = new Impl(chartView, plotMatrixView, statsView).init()
    res
  }

  private final class Impl[S <: Sys[S]](chartView: View[S], matrixView: PlotMatrixView[S], statsView: PlotStatsView[S])
    extends PlotView[S] with ComponentHolder[Component] {

    def workspace   : Workspace[S]  = matrixView.workspace
    def cursor      : stm.Cursor[S] = matrixView.cursor
    def undoManager : UndoManager   = matrixView.undoManager

    def plot(implicit tx: S#Tx): Plot[S] = matrixView.plotH()

    def init()(implicit tx: S#Tx): this.type = {
      deferTx(guiInit())
      this
    }

    private def guiInit(): Unit =
      component = new SplitPane(Orientation.Horizontal,
        left = chartView.component,
        right = new BoxPanel(Orientation.Horizontal) {
          contents += matrixView.component
          contents += statsView .component
        }
      )

    def dispose()(implicit tx: S#Tx): Unit = {
      chartView .dispose()
      matrixView.dispose()
      statsView .dispose()
    }
  }

  private final class PlotAssocView[S <: Sys[S]](protected val sourceH: stm.Source[S#Tx, Plot[S]], keyName: String)
                                                (implicit  workspace: Workspace[S], undoManager: UndoManager,
                                                 cursor: stm.Cursor[S])
    extends DimAssocViewImpl[S](keyName) {

    type Source[S1 <: Sys[S1]] = Plot[S1]

    protected def flavor: DragAndDrop.Flavor[PlotMappingDrag] = DragAndDrop.PlotMappingFlavor
  }

  private final class PlotMatrixView[S <: Sys[S]](protected val canSetMatrix: Boolean,
                                                  parentOpt: Option[SonificationView[S]])
                                                 (implicit val workspace: Workspace[S], val undoManager: UndoManager,
                                                  val cursor: stm.Cursor[S])
    extends MatrixAssocViewImpl[S](Vec(Plot.HKey, Plot.VKey)) { impl =>

    protected type Source[S1 <: Sys[S1]] = Plot[S1]

    protected def canRemoveMatrix: Boolean = false

    protected def editRemoveMatrix()(implicit tx: S#Tx): Option[UndoableEdit] = None

    protected def matrix(source: Source[S])(implicit tx: S#Tx): Matrix[S] = source.matrix

    protected def sourceSerializer: Serializer[S#Tx, S#Acc, Source[S]] = Plot.serializer[S]

    protected def mkAssocView(source: Source[S], key: String)(implicit tx: S#Tx): View[S] = {
      val dims      = source.dims
      val res       = new PlotAssocView[S](tx.newHandle(source), key)
      res.init(dims)
      res
    }

    private var _plotH: stm.Source[S#Tx, Plot[S]] = _
    private var _obsStatus = Option.empty[Disposable[S#Tx]]

    def plotH: stm.Source[S#Tx, Plot[S]] = _plotH

    private val synced  = Ref(false)
    // private var syncFut = Future.successful[Unit]

    @tailrec private def findReduce(m: Matrix[S], name: String)(implicit tx: S#Tx): Option[Reduce[S]] = m match {
      case Matrix.Var(vr) => findReduce(vr(), name)
      case r: Reduce[S] =>
        r.dim match {
          case ds: Dimension.Selection.Name[S] if ds.expr.value == name => Some(r)
          case _ => findReduce(r.in, name)
        }
      case _ => None
    }

    def init(plot: Plot[S])(implicit tx: S#Tx): this.type = {
      _plotH = tx.newHandle(plot)
      init()
      updateSource(Some(plot))

      _obsStatus = parentOpt.map(mkElapsedObservation)
      this
    }

    private type ElapsedReduce = (stm.Source[S#Tx, Reduce[S]], Future[Array[Float]])

    private val elapsedReduce = Ref(Option.empty[ElapsedReduce])

    private def readElapsedData(r: Reduce[S], mDim: String)(implicit tx: S#Tx): Option[ElapsedReduce] = {
      val dimIdx = r.in.dimensions.indexWhere(_.name == mDim)
      if (DEBUG) println(s"readElapsedData ${r.id}, $mDim, dimIdx = $dimIdx")
      if (dimIdx >= 0) {
        val dimKey  = r.in.getDimensionKey(dimIdx, useChannels = false)
        implicit val resolver = WorkspaceResolver[S]
        val reader  = dimKey.reader[S]()
        val len     = reader.numFrames.toInt
        val buf     = Array.ofDim[Float](1, len)
        val p       = Promise[Array[Float]]()
        tx.afterCommit {
          import at.iem.sysson.Stats.executionContext
          val fut = Future {
            reader.read(buf, 0, len)
            buf(0)
          }
          p.completeWith(fut)
          if (DEBUG) fut.onComplete {
            case Success(arr) => println(s"readElapsedData result: ${arr.toVector}")
            case Failure(ex)  => println(s"readElapsedData result: $ex")
          }
        }
        Some(tx.newHandle(r) -> p.future)

      } else None
    }

    private def mkElapsedObservation(sonifView: SonificationView[S])(implicit tx: S#Tx): Disposable[S#Tx] =
      sonifView.status.react { implicit tx =>
        upd => if (synced.get(tx.peer)) upd match {
          case AuralSonification.Elapsed(dim, ratio, dimValue) =>
            matrixView.matrix.foreach { m =>
              val sonif = sonifView.sonification
              sonif.sources.get(dim.variable.name).foreach { source =>
                source.dims.get(dim.name).foreach { mDimExpr =>
                  val mDim = mDimExpr.value
                  findReduce(m, mDim).foreach { r =>
                    r.op match {
                      case idxApp: Reduce.Op.Apply[S] =>
                        idxApp.index match {
                          case IntObj.Var(idxVr) =>
                            elapsedReduce.get(tx.peer) match {
                              case None /* | Some((src, _)) if src() != r */ =>
                                val e = readElapsedData(r, mDim)
                                elapsedReduce.set(e)(tx.peer)
                              case Some((src, _)) if src() != r =>
                                val e = readElapsedData(r, mDim)
                                elapsedReduce.set(e)(tx.peer)
                              case Some((_, data)) =>
                                data.value.foreach {
                                  case Success(buf) =>
                                    val idx0  = math.abs(java.util.Arrays.binarySearch(buf, dimValue))
                                    val idx   = if (idx0 >= 0) idx0 else -(idx0 + 1)
                                    if (DEBUG) println(s"Elapsed index for $dimValue is $idx")
                                    val idxOld = idxVr.value
                                    if (idx != idxOld) idxVr() = IntObj.newConst(idx)

                                  case _ =>
                                }
                            }

                          case _ =>
                        }
                      case _ =>
                    }
                  }
                }
              }
            }

          case _ =>
        }
      }

    override def dispose()(implicit tx: S#Tx): Unit = {
      super.dispose()
      _obsStatus.foreach(_.dispose())
    }

    protected def mkDimAssocTransferable(src: stm.Source[S#Tx, Source[S]], key0: String): Transferable =
      DragAndDrop.Transferable(DragAndDrop.PlotMappingFlavor)(new PlotMappingDrag {
        type S1 = S
        def source    : stm.Source[S#Tx, Source[S]] = src
        def key       : String                      = key0
        def workspace : Workspace[S]                = impl.workspace
      })

    protected def editDropMatrix(m: Matrix[S])(implicit tx: S#Tx): Option[UndoableEdit] =
      Matrix.Var.unapply(plotH().matrix).flatMap { vr =>
        import TypeCheckedTripleEquals._
        @tailrec def isRecursive(m0: Matrix[S]): Boolean = m0 match {
          case Matrix.Var(vr0) => if (vr0 === vr) true else isRecursive(vr0())
          case _ => false
        }

        if (isRecursive(m)) None else Some(EditVar("Set Matrix", vr, m))
      }

    override protected def mkTopComponent(c: Component): Component = {
      parentOpt.fold(c) { sonifView =>
        val ggSync          = new ToggleButton(null)
        val iconFun         = raphael.Shapes.Movie _
        ggSync.icon         = GUI.iconNormal  (iconFun)
        ggSync.disabledIcon = GUI.iconDisabled(iconFun)
        ggSync.tooltip      = "Synchronize with Sonification Playback"
        ggSync.reactions += {
          case ButtonClicked(_) => synced.single() = ggSync.selected
        }

        new FlowPanel(FlowPanel.Alignment.Leading)(c, ggSync)
      }
    }
  }
}
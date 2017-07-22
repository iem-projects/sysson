/*
 *  AbstractPlotViewImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2017 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.equal
import de.sciss.lucre.expr.StringObj
import de.sciss.lucre.matrix.{DataSource, Matrix}
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{defer, deferTx}
import de.sciss.lucre.{stm, event => evt}
import de.sciss.mellite.gui.ViewHasWorkspace
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.synth.proc.GenContext

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.stm.{Ref, TMap}
import scala.swing.Component
import scala.util.{Failure, Success, Try}

object AbstractPlotViewImpl {
  private final val DEBUG = false

  final class PlotData(val hName: String, val mUnits: String, val hData: Array[Float],
                       val vName: String, val hUnits: String, val vData: Array[Float],
                       val mName: String, val vUnits: String, val mData: Array[Array[Float]],
                       val is1D: Boolean)

  private final class Reader(mName: String, mUnits: String, mReader: Matrix.Reader,
                             hName: String, hUnits: String, hReader: Matrix.Reader,
                             vName: String, vUnits: String, vReader: Matrix.Reader,
                             is1D: Boolean)
    extends ProcessorImpl[PlotData, Reader] {

    private def readDim(r: Matrix.Reader): Array[Float] = {
      assert(r.numChannels == 1)
      val sz    = r.numFrames.toInt
      val res   = new Array[Float](sz)
      r.readFloat2D(Array(res), 0, sz)
      checkAborted()
      res
    }

    protected def body(): PlotData = {
//      // XXX TODO --- await is not good; should be able to flat-map processes
//      val maxDur = Duration(30, TimeUnit.SECONDS)
//      val hReader = Await.result(hReaderF, maxDur /* Duration.Inf */)
//      val vReader = Await.result(vReaderF, maxDur /* Duration.Inf */)
//      val mReader = Await.result(mReaderF, maxDur /* Duration.Inf */)

      val hData = readDim(hReader)
      val vData = readDim(vReader)

      val mCh   = mReader.numChannels
      val mSz   = mReader.numFrames.toInt
      val mData = Array.ofDim[Float](mCh, mSz)
      mReader.readFloat2D(mData, 0, mSz)
      checkAborted()

      new PlotData(
        hName = hName, hUnits = hUnits, hData = hData,
        vName = vName, vUnits = vUnits, vData = vData,
        mName = mName, mUnits = mUnits, mData = mData,
        is1D = is1D)
    }
  }
}
trait AbstractPlotViewImpl[S <: Sys[S]] extends ViewHasWorkspace[S] with ComponentHolder[Component] {
  import AbstractPlotViewImpl._

  // ---- abstract ----

  // called on EDT
  protected def updatePlot(data: PlotData): Unit

  // ---- impl ----

  implicit private[this] val resolver: DataSource.Resolver[S] = WorkspaceResolver[S]

  private[this] var _observers  = List.empty[Disposable[S#Tx]]
  private[this] var  _plotH: stm.Source[S#Tx, Plot[S]] = _
  private[this] val plotDimObs  = TMap.empty[String, Disposable[S#Tx]]

  private[this] val readerRef   = Ref(new Reader(null, null, null, null, null, null, null, null, null, is1D = false))

  private[this] val isReading   = Ref(false)
  private[this] val isDirty     = Ref(false)
  private[this] val isDisposed  = Ref(false)

  // checks if the shape is reducible in all but the provided dimensions
  private def checkShape1D(shape: Vec[Int], hIdx: Int, vIdx: Int): Boolean =
    shape.zipWithIndex.forall { case (n, i) => n == 1 || i == hIdx || i == vIdx }

  private def updateData(plot: Plot[S])(implicit tx: S#Tx): Unit =
    if (!isDisposed()) {
      if (!isReading()) updateData1(plot)
      else isDirty() = true
    }

  private def updateData1(plot: Plot[S])(implicit tx: S#Tx): Unit = {
    val m       = plot.matrix
    val dimMap  = plot.dims
    val dims    = m.dimensions
    val hName   = dimMap.get(Plot.HKey).map(_.value).getOrElse("?")
    val vName   = dimMap.get(Plot.VKey).map(_.value).getOrElse("?")
    import equal.Implicits._
    val hIdx0   = dims.indexWhere(_.name === hName)
    val vIdx0   = dims.indexWhere(_.name === vName)
    val mShape  = m.shape
    val shapeOk = checkShape1D(mShape, hIdx0, vIdx0)

    if (DEBUG) println(s"updateData. hIdx = $hIdx0, vIdx = $vIdx0, ok? $shapeOk")

    if (shapeOk) {
      //        println(s"h-unit: ${dims(hIdx).units}")
      //        println(s"v-unit: ${dims(vIdx).units}")

      import scala.concurrent.ExecutionContext.Implicits.global

      // this locks:
//      import SoundProcesses.executionContext
      implicit val context = GenContext[S]

      val hIdx      = if (hIdx0 >= 0) hIdx0 else if (vIdx0 >= 0) vIdx0 else 0
      val vIdx      = if (vIdx0 >= 0) vIdx0 else if (hIdx0 >= 0) hIdx0 else 0
      val is1D      = hIdx == vIdx

      val hKey      = m.prepareDimensionReader(hIdx, useChannels = false)
      val vKey      = m.prepareDimensionReader(vIdx, useChannels = false)
      val hReaderF  = hKey.reader()
      val vReaderF  = vKey.reader()
      val mName     = m.name  // or plot-obj attr name?
      val mReaderF  = m.reader(streamDim = hIdx)  // rows = channels, columns = frames
      val hDim      = dims(hIdx)
      val vDim      = dims(vIdx)
      val mUnits    = m   .units
      val hUnits    = hDim.units
      val vUnits    = vDim.units
      isReading()   = true
      tx.afterCommit {
        if (DEBUG) println("createReader")
        createReader(
          mName = mName, mUnits = mUnits, mReaderF = mReaderF,
          hName = hName, hUnits = hUnits, hReaderF = hReaderF,
          vName = vName, vUnits = vUnits, vReaderF = vReaderF,
          is1D = is1D)
      }
    }
  }

  private def createReader(mName: String, mUnits: String, mReaderF: Future[Matrix.Reader],
                           hName: String, hUnits: String, hReaderF: Future[Matrix.Reader],
                           vName: String, vUnits: String, vReaderF: Future[Matrix.Reader],
                           is1D: Boolean)
                          (implicit exec: ExecutionContext): Unit =
    for {
      mReader <- mReaderF
      hReader <- hReaderF
      vReader <- vReaderF
    } {
      if (DEBUG) println("create processor")
      val proc = new Reader(mName = mName, mUnits = mUnits, mReader = mReader,
        hName = hName, hUnits = hUnits, hReader = hReader, vName = vName, vUnits = vUnits,
        vReader = vReader, is1D = is1D)
      val oldProc = readerRef.single.swap(proc)

      oldProc.abort()
      if (!proc.aborted && !isDisposed.single()) {
        if (DEBUG) println("start processor")
        proc.start()
        proc.onComplete(readComplete)
      }
    }

  private def readComplete(tr: Try[PlotData]): Unit = {
    isReading.single() = false
    tr match {
      case Success(plotData) =>
        defer {
          if (DEBUG) println(s"X-Axis: ${plotData.hName}; ${plotData.hData.take(144).mkString(",")}")
          if (DEBUG) println(s"Y-Axis: ${plotData.vName}; ${plotData.vData.take(144).mkString(",")}")
          updatePlot(plotData)
        }
      case Failure(Processor.Aborted()) =>
      case Failure(ex) =>
        Console.err.println("Matrix reader failed:")
        ex.printStackTrace()
    }
    val repeat = isDirty.single.swap(false)
    if (DEBUG) println(s"readComplete. repeat = $repeat")
    if (repeat)
      cursor.step { implicit tx =>
        updateData(plotH())
      }
  }

  // ----

  protected final def addObserver(obs: Disposable[S#Tx]): Unit = _observers ::= obs

  protected final def plotH: stm.Source[S#Tx, Plot[S]] = _plotH

  def init(plot: Plot[S])(implicit tx: S#Tx): this.type = {
    _plotH = tx.newHandle(plot)

//    implicit val booleanEx  = BooleanObj
//    implicit val stringEx   = StringObj
//    implicit val doubleEx   = DoubleObj

    updateData(plot)  // XXX TODO --- might this be too early for sub-classes?

    _observers ::= plot.changed.react { implicit tx => u =>
      if (DEBUG) println("plot changed")
      updateData(u.plot)
      u.changes.foreach {
        case Plot.DimsChange(mu) => mu.changes.foreach {
          case evt.Map.Added   (key, value) => addPlotDim   (key, value)
          case evt.Map.Removed (key, value) => removePlotDim(key, value)
          case evt.Map.Replaced(key, before, now) =>
            removePlotDim(key, before)
            addPlotDim   (key, now   )
        }
        case _ =>
      }
    }

    plot.dims.iterator.foreach { case (key, value) => addPlotDim(key, value) }

    this
  }

  private def addPlotDim(key: String, value: StringObj[S])(implicit tx: S#Tx): Unit = {
    val obs = value.changed.react { implicit tx => _ =>
      updateData(_plotH())
    }
    plotDimObs.put(key, obs)
  }

  private def removePlotDim(key: String, value: StringObj[S])(implicit tx: S#Tx): Unit =
    plotDimObs.remove(key).foreach(_.dispose())

  def dispose()(implicit tx: S#Tx): Unit = if (!isDisposed.swap(true)) {
    _observers.foreach(_.dispose())
    val p = readerRef.get(tx.peer)
    plotDimObs.foreach(_._2.dispose())
    plotDimObs.clear()
    deferTx {
      p.abort()
    }
  }
}
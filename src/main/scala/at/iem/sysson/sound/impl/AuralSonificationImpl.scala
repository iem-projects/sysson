/*
 *  AuralSonificationImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package sound
package impl

import at.iem.sysson.graph.UserValue
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.Sys
import de.sciss.numbers
import de.sciss.synth.proc.AuralObj.State
import de.sciss.synth.proc.{UGenGraphBuilder => UGB, TimeRef, AuralContext, AuralObj}
import de.sciss.synth.proc.impl.{AsyncProcBuilder, SynthBuilder, AuralProcImpl, AuralProcDataImpl, ObservableImpl}
import de.sciss.lucre.{event => evt, stm}

import scala.concurrent.stm.TxnLocal

object AuralSonificationImpl extends AuralObj.Factory {
  AuralObj.addFactory(this)

  def typeID = Sonification.typeID
  type E[S <: evt.Sys[S]] = Sonification.Elem[S]

  def apply[S <: Sys[S]](obj: Sonification.Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = {
    logDebugTx(s"AuralSonification($obj)")
    val objH      = tx.newHandle(obj)
    val proc      = obj.elem.peer.proc
    val procData  = context.acquire[AuralObj.ProcData[S]](proc)(new ProcDataImpl[S].initSonif(obj))
    val res       = new Impl[S]
    val procView  = new ProcImpl[S](res).init(procData)
    res.init(objH, procView)
  }

  private def findSource[S <: Sys[S]](obj: Sonification.Obj[S], dimElem: graph.Dim)
                                     (implicit tx: S#Tx): Sonification.Source[S] = {
    val sonif   = obj.elem.peer
    val varKey  = dimElem.variable.name
    val source  = sonif.sources.get(varKey).getOrElse(sys.error(s"Missing source for key $varKey"))
    source
  }

  private def findDimIndex[S <: Sys[S]](source: Sonification.Source[S], dimElem: graph.Dim)
                                       (implicit tx: S#Tx): Int = {
    val dimKey  = dimElem.name
    val dimName = source.dims  .get(dimKey).getOrElse(sys.error(s"Missing dimension mapping for key $dimKey")).value
    val full    = source.matrix
    val dimIdx  = full.dimensions.indexWhere(_.name == dimName)
    if (dimIdx < 0) sys.error(s"Dimension $dimName not in matrix")
    dimIdx
  }

  private final class ProcDataImpl[S <: Sys[S]](implicit context: AuralContext[S])
    extends AuralProcDataImpl.Impl[S]() {

    private val sonifLoc = TxnLocal[Sonification.Obj[S]]() // cache-only purpose
    private var _obj: stm.Source[S#Tx, Sonification.Obj[S]] = _

    private def sonifCached()(implicit tx: S#Tx): Sonification.Obj[S] = {
      implicit val itx = tx.peer
      if (sonifLoc.isInitialized) sonifLoc.get
      else {
        val sonif = _obj()
        sonifLoc.set(sonif)
        sonif
      }
    }

    def initSonif(obj: Sonification.Obj[S])(implicit tx: S#Tx): this.type = {
      _obj = tx.newHandle(obj)
      init(obj.elem.peer.proc)
    }

    private def oldMatrixSpecs(req: UGB.Input, st: UGB.Incomplete[S]): Vec[MatrixPrepare.Spec] =
      st.acceptedInputs.get(req.key) match {
        case Some(mv: MatrixPrepare.Value) => mv.specs
        case Some(other) =>
          logDebug(s"For key ${req.key} found something other than MatrixPrepare.Value: $other")
          Vector.empty
        case _ => Vector.empty
      }

    override def requestInput[Res](req: UGB.Input { type Value = Res }, st: UGB.Incomplete[S])
                                  (implicit tx: S#Tx): Res = req match {
      case uv: graph.UserValue => UGB.Unit
      case dp: graph.Dim.Play  =>
        val oldSpecs = oldMatrixSpecs(req, st)
        //        val newSpecs = if (i.spec.isEmpty) newSpecs0 else {
        //          i.spec :: newSpecs0
        //        }
        val numCh     = 1 // a streaming dimension is always monophonic
        val newSpec   = MatrixPrepare.Spec(numChannels = numCh, maxFreq = dp.maxFreq, interp = dp.interp,
          streamDim = 0, isDim = true)
        MatrixPrepare.Value(oldSpecs :+ newSpec)

      case dv: graph.Dim.Values =>
        val oldSpecs = oldMatrixSpecs(req, st)

        val sonif     = sonifCached()
        val dimElem   = dv.dim
        val source    = findSource  (sonif , dimElem)
        val dimIdx    = findDimIndex(source, dimElem)
        val numCh     = source.matrix.shape.apply(dimIdx)
        val newSpec   = MatrixPrepare.Spec(numChannels = numCh, maxFreq = 0f, interp = 0, streamDim = -1, isDim = true)
        MatrixPrepare.Value(oldSpecs :+ newSpec)

      case vp: graph.Var.Play =>
        val oldSpecs = oldMatrixSpecs(req, st)

        val sonif     = sonifCached()
        val dimElem   = vp.time.dim
        val source    = findSource  (sonif , dimElem)
        val dimIdx    = findDimIndex(source, dimElem)
        val shape     = source.matrix.shape
        val numCh     = ((1L /: shape)(_ * _) / shape(dimIdx)).toInt
        println(s"graph.Var.Play - numChannels = $numCh")
        val newSpec   = MatrixPrepare.Spec(numChannels = numCh, maxFreq = vp.time.maxFreq, interp = vp.interp,
          streamDim = dimIdx, isDim = false)
        MatrixPrepare.Value(oldSpecs :+ newSpec)

      case _ => super.requestInput(req, st)
    }
  }

  private final class ProcImpl[S <: Sys[S]](sonifData: Impl[S])(implicit context: AuralContext[S])
    extends AuralProcImpl.Impl[S]() {

    override protected def buildSyncInput(b: SynthBuilder[S], keyW: UGB.Key, value: UGB.Value)
                                         (implicit tx: S#Tx): Unit = keyW match {
      case UserValue.Key(key) =>
        val sonif = sonifData.sonifCached().elem.peer
        sonif.controls.get(key).foreach { expr =>
          val ctlName = UserValue.controlName(key)
          b.setMap += ctlName -> expr.value
        }

      case _ => super.buildSyncInput(b, keyW, value)
    }

    override protected def buildAsyncInput(b: AsyncProcBuilder[S], keyW: UGB.Key, value: UGB.Value)
                                          (implicit tx: S#Tx): Unit = keyW match {
      case dim: graph.Dim =>
        value match {
          case MatrixPrepare.Value(specs) =>
            specs.zipWithIndex.foreach { case (spec, idx) =>
              addMatrixStream(b, dimElem = dim, spec = spec, idx = idx)
            }

          case _ => throw new IllegalStateException(s"Unsupported input request value $value")
        }

      case _ => super.buildAsyncInput(b, keyW, value)
    }

    private def addMatrixStream(b: AsyncProcBuilder[S], dimElem: graph.Dim, spec: MatrixPrepare.Spec, idx: Int)
                               (implicit tx: S#Tx): Unit = {
      // note: info-only graph elems not yet supported (or existent)
      import context.{server, workspaceHandle}
      import context.scheduler.cursor
      import spec.{isDim, streamDim}
      implicit val resolver = WorkspaceResolver[S]

      val source  = findSource(sonifData.obj(), dimElem)
      val full    = source.matrix
      val matrix  = if (isDim) {
        val dimIdx  = findDimIndex(source, dimElem)
        full.getDimensionKey(dimIdx, useChannels = streamDim < 0)
      } else {
        full.getKey(streamDim)
      }

      // val ctlName     = de.sciss.synth.proc.graph.stream.controlName(key, idx)
      //  val ctlName     = proc.graph.impl.Stream.controlName(key, idx)
      val bufSize     = if (spec.isEmpty) server.config.blockSize else {
        val maxFreq   = if (spec.maxFreq <= 0.0) 1.0 else spec.maxFreq
        val maxSpeed  = maxFreq / server.sampleRate
        val bufDur    = 1.5 * maxSpeed
        val minSz     = (2 * server.config.blockSize * math.max(1.0, maxSpeed)).toInt
        val bestSz    = math.max(minSz, (bufDur * server.sampleRate).toInt)
        import numbers.Implicits._
        val bestSzHi  = bestSz.nextPowerOfTwo
        val bestSzLo  = bestSzHi >> 1
        if (bestSzHi.toDouble/bestSz < bestSz.toDouble/bestSzLo) bestSzHi else bestSzLo
      }

      val key = MatrixPrepare.mkKey(dimElem, isDim = isDim) // graph.Dim.key(dimElem)
      val cfg = MatrixPrepare.Config(matrix = matrix, server = server, key = key, index = idx, bufSize = bufSize)
      val res = MatrixPrepare(cfg)
      b.resources ::= res
    }
  }

  private final class Impl[S <: Sys[S]]
    extends AuralObj[S] with ObservableImpl[S, AuralObj.State] {

    def typeID = Sonification.typeID

    private var procViewL: Disposable[S#Tx] = _

    private val sonifLoc = TxnLocal[Sonification.Obj[S]]() // cache-only purpose

    private var _obj: stm.Source[S#Tx, Sonification.Obj[S]] = _
    private var _procView: AuralObj[S] = _

    def obj: stm.Source[S#Tx, Sonification.Obj[S]] = _obj

    def init(obj: stm.Source[S#Tx, Sonification.Obj[S]], procView: AuralObj[S])(implicit tx: S#Tx): this.type = {
      _obj      = obj
      _procView = procView
      procViewL = procView.react { implicit tx => upd =>
        fire(upd) // just forward
      }
      this
    }

    def sonifCached()(implicit tx: S#Tx): Sonification.Obj[S] = {
      implicit val itx = tx.peer
      if (sonifLoc.isInitialized) sonifLoc.get
      else {
        val sonif = _obj()
        sonifLoc.set(sonif)
        sonif
      }
    }

    def prepare()(implicit tx: S#Tx): Unit = {
      logDebugTx("AuralSonification: prepare")
      _procView.prepare()
    }

    def play(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      logDebugTx("AuralSonification: play")
      _procView.play(timeRef)
    }

    def stop()(implicit tx: S#Tx): Unit = {
      logDebugTx("AuralSonification: stop")
      _procView.stop()
    }

    def state(implicit tx: S#Tx): State = {
      _procView.state
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      procViewL.dispose()
      _procView.dispose()
    }
  }
}

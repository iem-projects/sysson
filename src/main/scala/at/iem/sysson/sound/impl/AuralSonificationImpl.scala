/*
 *  AuralSonificationImpl.scala
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
package sound
package impl

import at.iem.sysson.graph.UserValue
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.lucre.{event => evt, stm}
import de.sciss.numbers
import de.sciss.synth.proc.AuralObj.State
import de.sciss.synth.proc.impl.{AsyncProcBuilder, AuralProcDataImpl, AuralProcImpl, SynthBuilder}
import de.sciss.synth.proc.{AuralContext, AuralObj, TimeRef, UGenGraphBuilder => UGB}
import org.scalautils.TypeCheckedTripleEquals

import scala.concurrent.stm.TxnLocal

object AuralSonificationImpl {
  def apply[S <: SSys[S]](obj: Sonification[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralSonification[S] = {
    logDebugTx(s"AuralSonification($obj)")
    val objH      = tx.newHandle(obj)
    val proc      = obj.proc
    val procData  = context.acquire[AuralObj.ProcData[S]](proc)(new ProcDataImpl[S].initSonif(obj))
    val res       = new Impl[S]
    val procView  = new ProcImpl[S](res).init(procData)
    res.init(objH, procView)
  }

  private def findSource[S <: SSys[S]](obj: Sonification[S], variable: graph.Var)
                                     (implicit tx: S#Tx): Sonification.Source[S] = {
    val sonif   = obj // .elem.peer
    val varKey  = variable.name
    val source  = sonif.sources.get(varKey).getOrElse(sys.error(s"Missing source for key $varKey"))
    source
  }

  private def findDimIndex[S <: SSys[S]](source: Sonification.Source[S], dimElem: graph.Dim)
                                       (implicit tx: S#Tx): Int = {
    val dimKey  = dimElem.name
    val dimName = source.dims  .get(dimKey).getOrElse(sys.error(s"Missing dimension mapping for key $dimKey")).value
    val full    = source.matrix
    import TypeCheckedTripleEquals._
    val dimIdx  = full.dimensions.indexWhere(_.name === dimName)
    if (dimIdx < 0) sys.error(s"Dimension $dimName not in matrix")
    dimIdx
  }

  private final class ProcDataImpl[S <: SSys[S]](implicit context: AuralContext[S])
    extends AuralProcDataImpl.Impl[S]() {

    private val sonifLoc = TxnLocal[Sonification[S]]() // cache-only purpose
    private var _obj: stm.Source[S#Tx, Sonification[S]] = _

    private def sonifCached()(implicit tx: S#Tx): Sonification[S] = {
      implicit val itx = tx.peer
      if (sonifLoc.isInitialized) sonifLoc.get
      else {
        val sonif = _obj()
        sonifLoc.set(sonif)
        sonif
      }
    }

    def initSonif(obj: Sonification[S])(implicit tx: S#Tx): this.type = {
      _obj = tx.newHandle(obj)
      init(obj.proc)
    }

    private def oldMatrixSpecs(req: UGB.Input, st: UGB.Incomplete[S]): Vec[MatrixPrepare.Spec] =
      st.acceptedInputs.get(req.key) match {
        case Some(mv: MatrixPrepare.Value) => mv.specs
        case Some(other) =>
          logDebug(s"For key ${req.key} found something other than MatrixPrepare.Value: $other")
          Vector.empty
        case _ => Vector.empty
      }

    private def addSpec(req: UGB.Input, st: UGB.Incomplete[S], spec: MatrixPrepare.Spec): MatrixPrepare.Value =
      MatrixPrepare.Value(oldMatrixSpecs(req, st) :+ spec)

    override def requestInput[Res](req: UGB.Input { type Value = Res }, st: UGB.Incomplete[S])
                                  (implicit tx: S#Tx): Res = req match {
      case _: graph.UserValue | _: graph.Dim.Size | _: graph.Var.Size | _: graph.Elapsed => UGB.Unit

      case dp: graph.Dim.Play  =>
        val numCh     = 1 // a streaming dimension is always monophonic
        val newSpec   = MatrixPrepare.Spec(numChannels = numCh, elem = dp, streamDim = 0)
        addSpec(req, st, newSpec)

      case dv: graph.Dim.Values =>
        val sonif     = sonifCached()
        val dimElem   = dv.dim
        val source    = findSource  (sonif , dv.variable)
        val dimIdx    = findDimIndex(source, dimElem)
        val numCh     = source.matrix.shape.apply(dimIdx)
        val newSpec   = MatrixPrepare.Spec(numChannels = numCh, elem = dv, streamDim = -1)
        addSpec(req, st, newSpec)

      case vp: graph.Var.Play =>
        val sonif     = sonifCached()
        val dimElem   = vp.time.dim
        val source    = findSource  (sonif , vp.variable)
        val dimIdx    = findDimIndex(source, dimElem)
        val shape     = source.matrix.shape
        val numCh     = ((1L /: shape)(_ * _) / shape(dimIdx)).toInt
        logDebug(s"graph.Var.Play - numChannels = $numCh")
        val newSpec   = MatrixPrepare.Spec(numChannels = numCh, elem = vp, streamDim = dimIdx)
        addSpec(req, st, newSpec)

      case vv: graph.Var.Values =>
        val sonif     = sonifCached()
        val source    = findSource(sonif, vv.variable)
        val numChL    = source.matrix.size
        if (numChL > 0xFFFF) sys.error(s"$vv - matrix too large ($numChL cells)")
        val numCh     = numChL.toInt
        val newSpec   = MatrixPrepare.Spec(numChannels = numCh, elem = vv, streamDim = -1)
        addSpec(req, st, newSpec)

      case av: graph.Var.Axis.Values =>
        val sonif     = sonifCached()
        val source    = findSource(sonif, av.variable)
        val streamIdx = findDimIndex(source, av.axis.variable.time.dim)
        val axisIdx   = findDimIndex(source, av.axis.asDim)
        val m         = source.matrix
        MatrixPrepare.ShapeAndIndex(shape = m.shape, streamIndex = streamIdx, axisIndex = axisIdx)

      case _ => super.requestInput(req, st)
    }
  }

  private final class ProcImpl[S <: SSys[S]](sonifData: Impl[S])(implicit context: AuralContext[S])
    extends AuralProcImpl.Impl[S]() {

    override protected def buildSyncInput(b: SynthBuilder[S], keyW: UGB.Key, value: UGB.Value)
                                         (implicit tx: S#Tx): Unit = keyW match {
      case UserValue.Key(key) =>
        val sonif = sonifData.sonifCached() // .elem.peer
        sonif.controls.get(key).foreach { expr =>
          val ctlName = UserValue.controlName(key)
          b.setMap += ctlName -> expr.value
        }

      case sz: graph.Dim.Size =>
        val sonif     = sonifData.sonifCached()
        val dimElem   = sz.dim
        val source    = findSource  (sonif , sz.dim.variable)
        val dimIdx    = findDimIndex(source, dimElem)
        val value     = source.matrix.shape.apply(dimIdx)
        val ctlName   = sz.ctlName
        b.setMap += ctlName -> value

      case sz: graph.Var.Size =>
        val sonif     = sonifData.sonifCached()
        val source    = findSource  (sonif , sz.variable)
        val value     = source.matrix.size
        val ctlName   = sz.ctlName
        b.setMap += ctlName -> value.toFloat

      case el: graph.Elapsed =>
        val resp = new graph.Elapsed.Responder(key = el.in.dim, status = sonifData.status, synth = b.node)
        // println("ADD ELAPSED RESPONDER USER")
        b.users ::= resp

      case _: graph.Var.Axis.Key =>   // nothing to be done

      case _ => super.buildSyncInput(b, keyW, value)
    }

    override protected def buildAsyncInput(b: AsyncProcBuilder[S], keyW: UGB.Key, value: UGB.Value)
                                          (implicit tx: S#Tx): Unit = value match {
      case MatrixPrepare.Value(specs) =>
        specs.zipWithIndex.foreach { case (spec, idx) =>
          addMatrixStream(b, spec = spec, idx = idx)
        }

      case _ => super.buildAsyncInput(b, keyW, value)
    }

    private def addMatrixStream(b: AsyncProcBuilder[S], spec: MatrixPrepare.Spec, idx: Int)
                               (implicit tx: S#Tx): Unit = {
      // note: info-only graph elems not yet supported (or existent)
      import context.scheduler.cursor
      import context.{server, workspaceHandle}
      import spec.{elem, streamDim}
      implicit val resolver = WorkspaceResolver[S]

      val source  = findSource(sonifData.obj(), elem.variable)
      val full    = source.matrix
      //      val matrix  = if (isDim) {
      //        val dimIdx  = findDimIndex(source, elem)
      //        full.getDimensionKey(dimIdx, useChannels = streamDim < 0)
      //      } else {
      //        full.getKey(streamDim)
      //      }
      val matrix = elem match {
        case dimGE: MatrixPrepare.DimGE =>
          val dimIdx = findDimIndex(source, dimGE.key)
          full.getDimensionKey(dimIdx, useChannels = streamDim < 0)

        case _ =>
          full.getKey(streamDim)
      }

      // val ctlName     = de.sciss.synth.proc.graph.stream.controlName(key, idx)
      //  val ctlName     = proc.graph.impl.Stream.controlName(key, idx)
      val bufSize     = /* if (spec.isEmpty) server.config.blockSize else */ {
        val maxFreq   = elem match {
          case st: MatrixPrepare.PlayGE => if (st.maxFreq <= 0.0) 1.0 else st.maxFreq
          case _ => 1.0
        }
        val maxSpeed  = maxFreq / server.sampleRate
        val bufDur    = 1.5 * maxSpeed
        val minSz     = (2 * server.config.blockSize * math.max(1.0, maxSpeed)).toInt
        val bestSz    = math.max(minSz, (bufDur * server.sampleRate).toInt)
        import numbers.Implicits._
        val bestSzHi  = bestSz.nextPowerOfTwo
        val bestSzLo  = bestSzHi >> 1
        if (bestSzHi.toDouble/bestSz < bestSz.toDouble/bestSzLo) bestSzHi else bestSzLo
      }

      val key = MatrixPrepare.mkKey(elem) // mkKeyOLD(dimElem, isDim = isDim) // graph.Dim.key(dimElem)
      val cfg = MatrixPrepare.Config(matrix = matrix, server = server, key = key, index = idx, bufSize = bufSize)
      val res = MatrixPrepare(cfg)
      b.resources ::= res
    }
  }

  private final class Impl[S <: SSys[S]]
    extends AuralSonification[S] with ObservableImpl[S, AuralObj.State] {

    def typeID = Sonification.typeID

    private var procViewL: Disposable[S#Tx] = _

    private val sonifLoc = TxnLocal[Sonification[S]]() // cache-only purpose

    private var _obj: stm.Source[S#Tx, Sonification[S]] = _
    private var _procView: AuralObj[S] = _

    def obj: stm.Source[S#Tx, Sonification[S]] = _obj

    object status
      extends ObservableImpl[S, AuralSonification.Update /* [S] */]
      with stm.Sink[S#Tx, AuralSonification.Update /* [S] */] {

      def update(u: AuralSonification.Update /* [S] */)(implicit tx: S#Tx): Unit = fire(u)
    }

    def init(obj: stm.Source[S#Tx, Sonification[S]], procView: AuralObj[S])(implicit tx: S#Tx): this.type = {
      _obj      = obj
      _procView = procView
      procViewL = procView.react { implicit tx => upd =>
        fire(upd) // just forward
      }
      this
    }

    def sonifCached()(implicit tx: S#Tx): Sonification[S] = {
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
      // status   .dispose()
    }
  }
}
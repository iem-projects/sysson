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
import de.sciss.lucre.matrix.Matrix
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.Sys
import de.sciss.numbers
import de.sciss.synth.proc
import de.sciss.synth.proc.AuralObj.State
import de.sciss.synth.proc.{UGenGraphBuilder => UGB, Proc, TimeRef, AuralContext, AuralObj}
import de.sciss.synth.proc.impl.{AsyncResource, SynthBuilder, AuralProcImpl, AuralProcDataImpl, ObservableImpl}
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
    val procData  = context.acquire[AuralObj.ProcData[S]](proc)(new ProcDataImpl[S].init(proc))
    val res       = new Impl[S]
    val procView  = new ProcImpl[S](res).init(procData)
    res.init(objH, procView)
  }

  private final class ProcDataImpl[S <: Sys[S]](implicit context: AuralContext[S])
    extends AuralProcDataImpl.Impl[S]() {

    override def requestInput[Res](req: UGB.Input { type Value = Res }, st: UGB.Incomplete[S])
                                  (implicit tx: S#Tx): Res = req match {
      case uv: graph.UserValue => UGB.Unit
      case dp: graph.Dim.Play  =>
        //        val newSpecs = if (i.spec.isEmpty) newSpecs0 else {
        //          i.spec :: newSpecs0
        //        }
        val numCh     = 1 // a streamed dimension is always monophonic
        val newSpecs  = Nil
        UGB.Input.Stream.Value(numChannels = numCh, specs = newSpecs)

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
    }

    override protected def buildAsyncInput(obj: Proc.Obj[S], keyW: UGB.Key, value: UGB.Value)
                                          (implicit tx: S#Tx): AsyncResource[S] = keyW match {
      case dim: graph.Dim =>
        value match {
          case UGB.Input.Stream.Value(numChannels, specs) =>
            addDimStream(obj, dimElem = dim /* key = graph.Dim.Play.key(dim) */, numChannels = numChannels, specs = specs)

          case _ => throw new IllegalStateException(s"Unsupported input request value $value")
        }

      case _ => super.buildAsyncInput(obj, keyW, value)
    }

    private def addDimStream(obj: Proc.Obj[S], dimElem: graph.Dim, numChannels: Int, specs: List[UGB.Input.Stream.Spec])
                            (implicit tx: S#Tx): AsyncResource[S] = {
      val infoSeq = if (specs.isEmpty) UGB.Input.Stream.EmptySpec :: Nil else specs
      import context.{server, workspaceHandle}
      import context.scheduler.cursor
      implicit val resolver = WorkspaceResolver[S]

      val sonif   = sonifData.sonifCached().elem.peer
      val varKey  = dimElem.variable.name
      val dimKey  = dimElem.name
      val source  = sonif.sources.get(varKey).getOrElse(sys.error(s"Missing source for key $varKey"))
      val dimName = source.dims.get(dimKey).getOrElse(sys.error(s"Missing dimension mapping for key $dimKey")).value
      val dim     = source.matrix.dimensions.find(_.name == dimName).getOrElse(sys.error(s"Dimension $dimName not in matrix"))
      ???

      infoSeq.zipWithIndex.foreach { case (info, idx) =>
        // val ctlName     = de.sciss.synth.proc.graph.stream.controlName(key, idx)
        //  val ctlName     = proc.graph.impl.Stream.controlName(key, idx)
        val bufSize     = if (info.isEmpty) server.config.blockSize else {
          val maxSpeed  = if (info.maxSpeed <= 0.0) 1.0 else info.maxSpeed
          val bufDur    = 1.5 * maxSpeed
          val minSz     = (2 * server.config.blockSize * math.max(1.0, maxSpeed)).toInt
          val bestSz    = math.max(minSz, (bufDur * server.sampleRate).toInt)
          import numbers.Implicits._
          val bestSzHi  = bestSz.nextPowerOfTwo
          val bestSzLo  = bestSzHi >> 1
          if (bestSzHi.toDouble/bestSz < bestSz.toDouble/bestSzLo) bestSzHi else bestSzLo
        }

        val matrix: Matrix.Key = ???
        val key: String = ???
        val cfg = MatrixPrepare.Config(matrix = matrix, server = server, key = key, index = idx, bufSize = bufSize)
        MatrixPrepare(cfg)
      }
      ???
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

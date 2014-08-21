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
import de.sciss.lucre.synth.{Buffer, Sys}
import de.sciss.numbers
import de.sciss.synth.ControlSet
import de.sciss.synth.proc.AuralObj.State
import de.sciss.synth.proc.{UGenGraphBuilder => UGB, AudioGraphemeElem, TimeRef, AuralContext, AuralObj}
import de.sciss.synth.proc.impl.AuralProcImpl.SynthBuilder
import de.sciss.synth.proc.impl.{StreamBuffer, AuralProcImpl, AuralProcDataImpl, ObservableImpl}
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

    override protected def buildInput(b: SynthBuilder[S], keyW: UGB.Key, value: UGB.Value)
                                     (implicit tx: S#Tx): Unit = keyW match {
      case UserValue.Key(key) =>
        val sonif = sonifData.sonifCached().elem.peer
        sonif.controls.get(key).foreach { expr =>
          val ctlName = UserValue.controlName(key)
          b.setMap += ctlName -> expr.value
        }

      case dim: graph.Dim =>
        value match {
          case UGB.Input.Stream.Value(numChannels, specs) =>
            addDimStream(b, key = graph.Dim.Play.key(dim), numChannels = numChannels, specs = specs)

          case _ => throw new IllegalStateException(s"Unsupported input request value $value")
        }

      case _ => super.buildInput(b, keyW, value)
    }

    // XXX TODO - was package private in previous SoundProcesses version
    private def dimPlayControlName(key: String, idx: Int): String = s"$$str${idx}_$key"

    private def addDimStream(b: SynthBuilder[S], key: String, numChannels: Int, specs: List[UGB.Input.Stream.Spec])
                            (implicit tx: S#Tx): Unit = {
      val infoSeq = if (specs.isEmpty) UGB.Input.Stream.EmptySpec :: Nil else specs
      import context.server

      infoSeq.zipWithIndex.foreach { case (info, idx) =>
        // val ctlName     = de.sciss.synth.proc.graph.stream.controlName(key, idx)
        val ctlName     = dimPlayControlName(key, idx)
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

        val a         = ??? : AudioGraphemeElem[S]
        val audioElem = a.peer
        val spec      = audioElem.spec
        val path      = audioElem.artifact.value.getAbsolutePath
        val offset    = audioElem.offset  .value
        // val gain      = audioElem.gain    .value
        val rb        = if (info.isNative) {
          Buffer.diskIn(server)(
            path          = path,
            startFrame    = offset,
            numFrames     = bufSize,
            numChannels   = spec.numChannels
          )
        } else {
          val __buf = Buffer(server)(numFrames = bufSize, numChannels = spec.numChannels)
          val trig = new StreamBuffer(key = key, idx = idx, synth = b.synth, buf = __buf, path = path,
            fileFrames = spec.numFrames, interp = info.interp)
          trig.install()
          __buf
        }

        b.setMap      += (ctlName -> /* Seq( */ rb.id.toFloat /*, gain.toFloat) */: ControlSet)
        b.dependencies ::= rb
      }
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

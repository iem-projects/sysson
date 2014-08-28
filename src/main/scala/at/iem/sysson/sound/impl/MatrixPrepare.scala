/*
 *  MatrixPrepare.scala
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

import de.sciss.lucre.matrix.{DataSource, Matrix}
import de.sciss.lucre.stm
import de.sciss.lucre.synth.{Server, Sys, Buffer}
import de.sciss.model.impl.ModelImpl
import de.sciss.processor.Processor
import de.sciss.processor.impl.{FutureProxy, ProcessorImpl}
import de.sciss.synth.proc
import de.sciss.synth.proc.{UGenGraphBuilder => UGB, SoundProcesses}
import de.sciss.synth.proc.impl.{StreamBuffer, SynthBuilder, AsyncResource}

import scala.concurrent.{Future, blocking, Await, duration}
import duration.Duration

object MatrixPrepare {
  type Spec = UGB.Input.Stream.Spec
  val  Spec = UGB.Input.Stream.Spec

  // same as Stream.Value, but asynchronous preparation
  final case class Value(numChannels: Int, specs: List[Spec], streamDim: Int) extends UGB.Value {
    override def productPrefix = "MatrixPrepare.Value"
    def async = true
  }

  /** The configuration of the buffer preparation.
    *
    * The `key` and `index` arguments will be fed into `Stream.controlName(key, index)`
    * to generate the control name, and will be passed to `StreamBuffer`.
    *
    * @param matrix   the matrix to convert into an audio file
    * @param key      the key used for setting the synth control eventually
    * @param index    the index into the alternative versions of the key, used for setting the synth control eventually
    * @param bufSize  the desired size for the resulting streaming buffer
    */
  case class Config(matrix: Matrix.Key, server: Server, key: String, index: Int, bufSize: Int)

  /** Creates and launches the process. */
  def apply[S <: Sys[S]](config: Config)(implicit tx: S#Tx, resolver: DataSource.Resolver[S],
                                         cursor: stm.Cursor[S]): AsyncResource[S] = {
    import config._
    logDebugTx(s"MatrixPrepare($config)")
    val cache = AudioFileCache.acquire(matrix)
    if (cache.isCompleted) {
      new SyncImpl[S](config, cache)
    } else {
      import SoundProcesses.executionContext
      val res = new AsyncImpl[S](config, cache)
      tx.afterCommit(res.start())
      res
    }
  }

  private abstract class Impl[S <: Sys[S]] extends AsyncResource[S] {
    protected val config: Config

    override def toString = s"MatrixPrepare(${config.matrix})@${hashCode().toHexString}"

    protected def cache: Future[AudioFileCache.Value]

    final def install(b: SynthBuilder[S])(implicit tx: S#Tx): Unit = {
      val value        = cache.value.get.get
      val numFrames    = value.spec.numFrames
      val numChannels  = value.spec.numChannels
      import config.{key, index, server, bufSize}
      val buf          = Buffer(server)(numFrames = bufSize, numChannels = numChannels)
      val ctlName      = proc.graph.impl.Stream.controlName(key, index)  // proc.graph.Buffer.controlName(key)
      val trig         = new StreamBuffer(key = key, idx = index, synth = b.synth, buf = buf,
          path = value.file.getAbsolutePath, fileFrames = numFrames, interp = 1 /* info.interp */)
      trig.install()
      b.setMap        += ctlName -> buf.id
      b.dependencies ::= buf
      b.synth.onEndTxn { implicit tx =>
        AudioFileCache.release(config.matrix)
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      tx.afterCommit(abort())
      AudioFileCache.release(config.matrix)
    }
  }

  private final class SyncImpl[S <: Sys[S]](protected val config: Config,
                                            protected val cache: Future[AudioFileCache.Value])
    extends Impl[S] with ModelImpl[Processor.Update[Product, AsyncResource[S]]] with FutureProxy[Product] {

    protected def peerFuture = cache

    def progress: Double = 1.0

    def abort() = ()
  }

  private final class AsyncImpl[S <: Sys[S]](protected val config: Config,
                                             protected val cache: Future[AudioFileCache.Value])
    extends Impl[S] with ProcessorImpl[AudioFileCache.Value, AsyncResource[S]] {

    protected def body(): AudioFileCache.Value = {
      val res = blocking {
        Await.result(cache, Duration.Inf)
      }
      checkAborted()
      progress = 1.0
      res
    }
  }
}
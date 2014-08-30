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
import de.sciss.synth
import de.sciss.synth.{UGenInLike, GE, proc}
import de.sciss.synth.proc.{UGenGraphBuilder => UGB, SoundProcesses}
import de.sciss.synth.proc.impl.{StreamBuffer, SynthBuilder, AsyncResource}

import scala.concurrent.{Future, blocking, Await, duration}
import duration.Duration

object MatrixPrepare {
  //  type Spec = UGB.Input.Stream.Spec
  //  val  Spec = UGB.Input.Stream.Spec

  final case class Spec(maxFreq: Double, interp: Int) {
    /** Empty indicates that the stream is solely used for information
      * purposes such as `BufChannels`.
      */
    def isEmpty: Boolean = interp == 0

    //    /** Native indicates that the stream will be transported by the UGen
    //      * itself, i.e. via `DiskIn` or `VDiskIn`.
    //      */
    //    def isNative: Boolean = interp == -1

    override def productPrefix = "MatrixPrepare.Spec"
  }

  /** The value of the `UGenGraphBuilder` request.
    * Similar to `Stream.Value`, but using asynchronous preparation.
    *
    * @param numChannels  the number of channels in the audio file
    * @param specs        the stream performance specifications
    * @param streamDim    the stream dimension (or `-1` for scalar)
    * @param isDim        if `true`, produce values for a dimension, if `false` for a matrix
    */
  final case class Value(numChannels: Int, specs: List[Spec], streamDim: Int, isDim: Boolean) extends UGB.Value {
    override def productPrefix = "MatrixPrepare.Value"

    def async = true

    def isStreaming = streamDim >= 0
  }

  /** Creates and expands the synth-graph fragment for a matrix. This must be
    * called in the expansion process. It will invoke `UGenGraphBuilder.requestInput`.
    * It handles both the streaming and the scalar case.
    *
    * @param in     the input element to be streamed
    * @param key    the key of the matrix; this will be fed into `Stream.controlName`
    * @param freq   the frequency (Hz) at which to stream the data. For non-streaming,
    *               this value is ignored (and should be just set to zero)
    * @param interp the interpolation for the streaming: 1 step, 2 linear, 4 cubic.
    *               For non-streaming, this value is ignored (and should be just set to zero).
    */
  def makeUGen(in: UGB.Input { type Value = MatrixPrepare.Value }, key: String, freq: GE, interp: Int): UGenInLike = {
    val b         = UGB.get
    val info      = b.requestInput(in)

    import synth._
    import ugen._

    if (info.isStreaming) {
      val bufSr   = SampleRate.ir // note: VDiskIn uses server sample rate as scale base
      val speed   = freq / bufSr
      val idx     = /* if (spec.isEmpty) 0 else */ info.specs.size - 1
      val ctlName = proc.graph.impl.Stream.controlName(key, idx)
      val buf     = ctlName.ir(0f)
      val numCh   = info.numChannels
      // only for dimensions: assert(numCh == 1)
      StreamBuffer.makeUGen(key = key, idx = idx, buf = buf, numChannels = numCh, speed = speed, interp = interp)
    } else {
      val numCh   = info.numChannels
      val ctlName = graph.Dim.Values.controlName(key)
      val buf     = ctlName.ir(0f)
      BufRd.ir(numCh, buf = buf, loop = 0, interp = 1)
    }
  }

  /** The configuration of the buffer preparation.
    *
    * The `key` and `index` arguments will be fed into `Stream.controlName(key, index)`
    * to generate the control name, and will be passed to `StreamBuffer`.
    *
    * @param matrix       the matrix to convert into an audio file
    * @param key          the key used for setting the synth control eventually
    * @param index        the index into the alternative versions of the key, used for setting the synth control eventually
    * @param bufSize      the desired size for the resulting streaming buffer. If not-streaming, this value
    *                     is ignored, and should be set to zero
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
      val value         = cache.value.get.get
      val numFrames     = value.spec.numFrames
      val numChannels   = value.spec.numChannels
      import config.{key, index, server, bufSize, matrix}
      val isStreaming   = matrix.isStreaming
      val bufSize1      = if (isStreaming) bufSize else numFrames.toInt
      val buf           = Buffer(server)(numFrames = bufSize1, numChannels = numChannels)
      val ctlName       = proc.graph.impl.Stream.controlName(key, index)  // proc.graph.Buffer.controlName(key)
      val path          = value.file.getAbsolutePath
      if (isStreaming) {
        val trig = new StreamBuffer(key = key, idx = index, synth = b.synth, buf = buf,
          path = path, fileFrames = numFrames, interp = 1 /* info.interp */)
        trig.install()
      } else {
        buf.read(path)
      }
      b.setMap         += ctlName -> buf.id
      b.dependencies  ::= buf
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
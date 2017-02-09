/*
 *  MatrixPrepare.scala
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
package sound
package impl

import at.iem.sysson.graph.{MatrixLike, MatrixTooLargeException}
import de.sciss.lucre.matrix.{DataSource, Matrix}
import de.sciss.lucre.stm
import de.sciss.lucre.synth.{Buffer, NodeRef, Server, Sys}
import de.sciss.model.impl.ModelImpl
import de.sciss.processor.Processor
import de.sciss.processor.impl.{FutureProxy, ProcessorImpl}
import de.sciss.synth
import de.sciss.synth.proc.impl.{AsyncResource, StreamBuffer}
import de.sciss.synth.proc.{SoundProcesses, UGenGraphBuilder => UGB}
import de.sciss.synth.{AudioRated, ScalarRated, UGenInLike, proc}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, blocking}

object MatrixPrepare {
  //  type Spec = UGB.Input.Stream.Spec
  //  val  Spec = UGB.Input.Stream.Spec

  object IsStreaming {
    case object No extends IsStreaming
    case class Yes(freq: synth.GE, interp: Int) extends IsStreaming
  }
  sealed trait IsStreaming

  trait GE extends synth.GE.Lazy with UGB.Input {
    def variable: graph.MatrixLike
  }

  sealed trait InputGE extends GE  {
    final type Value = MatrixPrepare.Value

    private[sysson] def isDimension: Boolean
    private[sysson] def isStreaming: IsStreaming
    private[sysson] def dimOption: Option[graph.Dim]
    private[sysson] def maxNumChannels: Int

    final protected def makeUGens: UGenInLike = MatrixPrepare.makeUGen(this)
  }

  trait PlayGE extends InputGE with AudioRated {
    protected def freq: synth.GE
    def maxFreq: Double
    protected def interp: Int
    private[sysson] final def isStreaming = IsStreaming.Yes(freq, interp)
  }

  trait ValuesGE extends InputGE with ScalarRated {
    private[sysson] final def isStreaming = IsStreaming.No
  }

  trait DimGE extends InputGE {
    final type Key = graph.Dim

    protected def dim: graph.Dim

    final def key: Key      = dim
    final def variable: MatrixLike = dim.variable

    private[sysson] final def isDimension = true
    private[sysson] final def dimOption: Option[graph.Dim] = Some(key)
  }

  trait VarGE extends InputGE {
    private[sysson] final def isDimension = false
  }

  //  * @param maxFreq      maximum playback sample-rate
  //  * @param interp       interpolation type (1 none, 2 linear, 4 cubic)
  //  * @param isDim        if `true`, produce values for a dimension, if `false` for a matrix

  /** A `UGenGraphBuilder.Input.Value` requested by the `Var.Axis` elements.
    *
    * @param shape        the shape of the matrix
    * @param streamIndex  the index of the streaming dimension
    * @param axisIndex    the index of the axis dimension
    */
  final case class ShapeAndIndex(shape: Vec[Int], streamIndex: Int, axisIndex: Int) extends UGB.Value {
    def async: Boolean = false
  }

  /** @param numChannels  the number of channels in the audio file
    * @param streamDim    the stream dimension (or `-1` for scalar)
    */
  final case class Spec(numChannels: Int, elem: InputGE, streamDim: Int) {
    //    /** Empty indicates that the stream is solely used for information
    //      * purposes such as `BufChannels`.
    //      */
    //    def isEmpty: Boolean = interp == 0

    def isStreaming: Boolean = streamDim >= 0

    override def toString = s"$productPrefix(numChannels = $numChannels, elem = $elem, streamDim = $streamDim)"

    // override def productPrefix = "MatrixPrepare.Spec"
  }

  /** The value of the `UGenGraphBuilder` request.
    * Similar to `Stream.Value`, but using asynchronous preparation.
    *
    * @param specs        the stream performance specifications
    */
  final case class Value(specs: Vec[Spec]) extends UGB.Value {
    override def productPrefix = "MatrixPrepare.Value"

    def async = true

    // def isStreaming = streamDim >= 0

    override def toString = s"$productPrefix(${specs.mkString("[", ", ", "]")})"
  }

  //  def mkKeyOLD(dim: graph.Dim, isDim: Boolean): String = {
  //    val prefix = if (isDim) "dim" else "var"
  //    s"${prefix}_${dim.variable.name}_${dim.name}"
  //  }

  def mkCtlName(key: String, idx: Int, isStreaming: Boolean): String =
    if (isStreaming) {
      proc.graph.impl.Stream.controlName(key, idx)
    } else {
      s"$$val${idx}_$key"
    }
  
  def mkKey(in: InputGE): String = in.dimOption.fold(s"var_${in.variable.name}") { dim =>
    val prefix = if (in.isDimension) "dim" else "var"
    s"${prefix}_${in.variable.name}_${dim.name}"
  }

  def makeUGen(in: InputGE): UGenInLike = {
    val b         = UGB.get
    val info      = b.requestInput(in)
    val spec      = info.specs.last

    import synth._
    import ugen._
    import Ops.stringToControl

    val idx     = /* if (spec.isEmpty) 0 else */ info.specs.size - 1
    val key     = mkKey(in)
    val ctlName = mkCtlName(key = key, idx = idx, isStreaming = spec.isStreaming)
    val numCh   = spec.numChannels
    if (numCh > in.maxNumChannels) {
      val vName = in.variable.name
      val name  = if (!in.isDimension) vName else in.dimOption.fold(vName)(dim => s"${vName}_${dim.name}")
      throw new MatrixTooLargeException(name, numCh, in.maxNumChannels)
    }

    in.isStreaming match {
      case IsStreaming.Yes(freq, interp) =>
        val bufSr   = SampleRate.ir // note: VDiskIn uses server sample rate as scale base
        val speed   = freq / bufSr
        val buf     = ctlName.ir(0f)
        // only for dimensions: assert(numCh == 1)
        StreamBuffer.makeUGen(key = key, idx = idx, buf = buf, numChannels = numCh, speed = speed, interp = interp)

      case IsStreaming.No =>
        val buf     = ctlName.ir(0f)
        BufRd.ir(numCh, buf = buf, loop = 0, interp = 1)
    }
  }

  //  /** Creates and expands the synth-graph fragment for a matrix. This must be
  //    * called in the expansion process. It will invoke `UGenGraphBuilder.requestInput`.
  //    * It handles both the streaming and the scalar case.
  //    *
  //    * @param in     the input element to be streamed
  //    * @param key    the key of the matrix; this will be fed into `Stream.controlName`
  //    * @param freq   the frequency (Hz) at which to stream the data. For non-streaming,
  //    *               this value is ignored (and should be just set to zero)
  //    * @param interp the interpolation for the streaming: 1 step, 2 linear, 4 cubic.
  //    *               For non-streaming, this value is ignored (and should be just set to zero).
  //    */
  //  def makeUGenOLD(in: UGB.Input { type Value = MatrixPrepare.Value }, key: String, freq: synth.GE, interp: Int): UGenInLike = {
  //    val b         = UGB.get
  //    val info      = b.requestInput(in)
  //    val spec      = info.specs.last
  //
  //    import synth._
  //    import ugen._
  //
  //    val idx     = /* if (spec.isEmpty) 0 else */ info.specs.size - 1
  //    val ctlName = mkCtlName(key = key, idx = idx, isStreaming = spec.isStreaming)
  //
  //    if (spec.isStreaming) {
  //      val bufSr   = SampleRate.ir // note: VDiskIn uses server sample rate as scale base
  //      val speed   = freq / bufSr
  //      // val ctlName = proc.graph.impl.Stream.controlName(key, idx)
  //      val buf     = ctlName.ir(0f)
  //      val numCh   = spec.numChannels
  //      // only for dimensions: assert(numCh == 1)
  //      StreamBuffer.makeUGen(key = key, idx = idx, buf = buf, numChannels = numCh, speed = speed, interp = interp)
  //    } else {
  //      val numCh   = spec.numChannels
  //      // val ctlName = graph.Dim.Values.controlName(key)
  //      val buf     = ctlName.ir(0f)
  //      BufRd.ir(numCh, buf = buf, loop = 0, interp = 1)
  //    }
  //  }

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

    final def install(b: NodeRef.Full[S])(implicit tx: S#Tx): Unit = {
      val value         = cache.value.get.get
      val numFrames     = value.spec.numFrames
      val numChannels   = value.spec.numChannels
      import config.{bufSize, index, key, matrix, server}
      val isStreaming   = matrix.isStreaming
      val bufSize1      = if (isStreaming) bufSize else numFrames.toInt
      val buf           = Buffer(server)(numFrames = bufSize1, numChannels = numChannels)
      val path          = value.file.getAbsolutePath
      val ctlName       = mkCtlName(key = key, idx = index, isStreaming = isStreaming)
      val syn           = b.node /* b.synth */
      if (isStreaming) {
        // XXX TODO - dirty workaround for Turbulence. Generally enable loop. Read start frame
        val startFrame = 0L
//          if (key.contains("!1850")) {
//          val res = VoiceStructure.currentFrame1850 % numFrames
//          // println(s"MatrixPrepare : install $key - startFrame = $res")
//          res
//        } else if (key.contains("!2001")) {
//          val res = VoiceStructure.currentFrame2001 % numFrames
//          res
//        } else if (key.contains("!rad")) {
//          // one frame is one year
//          val res = (VoiceStructure.currentFrame1850 / 12) % numFrames
//          res
//        } else {
//          // println(s"MatrixPrepare : install $key - no startFrame")
//          0L
//        }

        val trig  = new StreamBuffer(key = key, idx = index, synth = syn, buf = buf,
          path = path, fileFrames = numFrames, interp = 1, startFrame = startFrame, loop = true, resetFrame = 0L)
        b.addUser(trig) // XXX TODO --- CORRECT?
        // b.users ::= trig // NOT: b.addUser(trig) -- which is keyed!
        // trig.install()
      } else {
        buf.read(path)
      }
      b.addControl(ctlName -> buf.id)  // XXX TODO --- CORRECT?
      // b.setMap         += ctlName -> buf.id
      b.addResource(buf) // XXX TODO --- CORRECT?
      // b.dependencies  ::= buf
      syn.onEndTxn { implicit tx =>
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

    protected def peerFuture: Future[AudioFileCache.Value] = cache

    def progress: Double = 1.0

    def abort(): Unit = ()
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
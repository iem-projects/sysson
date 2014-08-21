/*
 *  AudioFileCache.scala
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

import de.sciss.filecache
import de.sciss.serial.{DataOutput, DataInput, ImmutableSerializer}
import de.sciss.file._
import de.sciss.synth.proc.Grapheme
import de.sciss.synth.io.{AudioFileSpec, AudioFile}
import de.sciss.lucre.event.Sys
import scala.concurrent.stm.TMap
import scala.util.control.NonFatal
import scala.concurrent.stm.atomic
import de.sciss.lucre.matrix.{Matrix, DataSource}
import de.sciss.mellite.Workspace
import scala.concurrent.{Future, ExecutionContext, blocking}
import de.sciss.lucre.{matrix, stm}

object AudioFileCache {
  private val instance = {
    val config = matrix.AudioFileCache.Config()
    config.folder = dataDir / "cache" // XXX TODO should read prefs
    matrix.AudioFileCache(config)
  }

  def acquire[S <: Sys[S]](key: Matrix.Key)(implicit tx: S#Tx, resolver: DataSource.Resolver[S],
                                            cursor: stm.Cursor[S]): Future[matrix.AudioFileCache.Value] =
    instance.acquire(key)

//  private val KEY_COOKIE  = 0x6166636B  // "afck"
//  private val VAL_COOKIE  = 0x61666376  // "afcv"
//
//  private val DEBUG   = false
//
//  private def debug(what: => String): Unit = if (DEBUG) println(s"<cache> $what")
//
//  type Result = Grapheme.Value.Audio
//
//  implicit val executionContext: ExecutionContext = ExecutionContext.global
//
//  import de.sciss.lucre.matrix.Serializers.RangeSerializer
//  private val parentsSer  = ImmutableSerializer.list[String]
//  private val sectionSer  = ImmutableSerializer.indexedSeq[Range]
//
//  private object CacheKey {
//    implicit object Serializer extends ImmutableSerializer[CacheKey] {
//      def read(in: DataInput): CacheKey = {
//        val cookie = in readInt()
//        require(cookie == KEY_COOKIE,
//          s"Unexpected cookie (expected ${KEY_COOKIE.toHexString}, found ${cookie.toHexString})")
//        val path      = in readUTF()
//        val parents   = parentsSer.read(in)
//        val variable  = in readUTF()
//        val section   = sectionSer.read(in)
//        val streamDim = in readInt()
//        CacheKey(file = file(path), parents = parents, variable = variable, section = section, streamDim = streamDim)
//      }
//
//      def write(v: CacheKey, out: DataOutput): Unit = {
//        import v._
//        out writeInt  KEY_COOKIE
//        out writeUTF  file.path
//        parentsSer.write(parents, out)
//        out writeUTF  variable
//        sectionSer.write(section, out)
//        out writeInt  streamDim
//      }
//    }
//  }
//  /* Key part of the cache
//   *
//   * @param file      the NetCDF file
//   * @param parents   the parent groups of the variable, excluding the root group
//   * @param variable  the variable name
//   * @param section   the sectioning of the variable
//   * @param streamDim  the dimension index to stream or `-1`
//   */
//  private case class CacheKey(file: File, parents: List[String], variable: String, section: Vec[Range],
//                              streamDim: Int) {
//    // lazy val spec: AudioFileSpec = keyToSpec(this)
//  }
//
//  private def sectionToSpec(vs: VariableSection, streamDim: Int /* , key: CacheKey */): AudioFileSpec = {
//    // import key._
//    val isStreaming   = streamDim >= 0
//    val shape         = vs.shape
//    val numFrames     = if (!isStreaming) 1 else shape(streamDim)
//    val size          = (1L /: shape)((sum, sz) => sum * sz)
//    val numChannelsL  = size / numFrames
//    require(numChannelsL <= 0xFFFF, s"The number of channels ($numChannelsL) is larger than supported")
//    val numChannels   = numChannelsL.toInt
//    logDebug(s"sectionToSpec($vs): shape = $shape, numFrames = $numFrames, size = $size, numChannels = $numChannels")
//    AudioFileSpec(numFrames = numFrames, numChannels = numChannels, sampleRate = 44100 /* rate */)
//  }
//
//  private object CacheValue {
//    implicit object Serializer extends ImmutableSerializer[CacheValue] {
//      def write(v: CacheValue, out: DataOutput): Unit = {
//        import v._
//        out writeInt  VAL_COOKIE
//        out writeLong netSize
//        out writeLong netModified
//        out writeUTF  data.getPath
//        AudioFileSpec.Serializer.write(spec, out)
//      }
//
//      def read(in: DataInput): CacheValue = {
//        val cookie = in.readInt()
//        require(cookie == VAL_COOKIE, s"Serialized version $cookie does not match $VAL_COOKIE")
//        val netSize       = in.readLong()
//        val netModified   = in.readLong()
//        val data          = new File(in.readUTF())
//        val spec          = AudioFileSpec.Serializer.read(in)
//        CacheValue(netSize = netSize, netModified = netModified, data = data, spec = spec)
//      }
//    }
//  }
//  private case class CacheValue(netSize: Long, netModified: Long, data: File, spec: AudioFileSpec) {
//    override def toString =
//      s"$productPrefix(size = $netSize, lastModified = ${new java.util.Date(netModified)}, data = ${data.getName})"
//  }
//
//  // note: `S#Tx` is only needed for the `name` method. This is a constant in DataSource.Variable,
//  // so if necessary, we could remove `S#Tx` and add a `nameConst` method.
//  private def sectionToKey[S <: Sys[S]](source: DataSource.Variable[S], section: Vec[Range],
//                                        streamDim: Int)(implicit tx: S#Tx): CacheKey = {
//    // val v = section.variable
//    val f = file(source.source.path) // v.file.path
//
//    //    val sectClosed: Vec[Range] = section.ranges.map { r =>
//    //      Range.inclusive(r.first(), r.last(), r.stride())
//    //    }
//    CacheKey(f, source.parents, source.name, section /* sectClosed */, streamDim)
//  }
//
//  private lazy val folder: File = dataDir / "cache"
//
//  /*
//    The cache is organised as follows:
//    - the lookup part of the key is the NetCDF file (`get` takes a `NetcdfFile`, we actually just use its path).
//    - the value completion of the key is an instance of `CacheValue`, which maintains verifiable information about
//     the NetCDF file's identity (size and modification date), along with a pointer `data` to the actually
//     generated stats file which is associated with the cache
//
//    This stats file is a straight forward serialization of the `Stats` trait.
//
//   */
//  private lazy val cache = {
//    val config              = filecache.Config[CacheKey, CacheValue]()
//    config.capacity         = filecache.Limit(count = 500, space = 10L * 1024 * 1024 * 1024)  // 10 GB
//    config.accept           = (key, value) => {
//      val res = key.file.lastModified() == value.netModified && key.file.length() == value.netSize
//      debug(s"accept key = ${key.file.name} (lastModified = ${new java.util.Date(key.file.lastModified())}}), value = $value? $res")
//      res
//    }
//    config.space            = (_  , value) => value.data.length()
//    config.evict            = (_  , value) => {
//      debug(s"evict $value")
//      value.data.delete()
//    }
//    config.folder           = folder
//    config.executionContext = AudioFileCache.executionContext
//    atomic { implicit tx =>
//      filecache.TxnProducer(config)
//    }
//  }
//
//  final private class Entry(val useCount: Int = 1, val future: Future[Result]) {
//    def inc = new Entry(useCount + 1, future)
//    def dec = new Entry(useCount - 1, future)
//  }
//
//  private val map = TMap.empty[CacheKey, Entry]
//
//  // def acquire(section: VariableSection, streamDim: Int): Future[Result]
//
//  private def produceValue[S <: Sys[S]](workspace: Workspace[S], source: DataSource.Variable[S],
//                                        section: Vec[Range], streamDim: Int)(implicit cursor: stm.Cursor[S]): CacheValue = {
//    implicit val resolver = WorkspaceResolver(workspace)
//    val v             = /* workspace. */ cursor.step { implicit tx => source.data() }
//    val vs            = VariableSection(v, section.map(OpenRange.closed))
//    val arr           = vs.readSafe()
//
//    // cf. Arrays.txt for (de-)interleaving scheme
//    val spec          = sectionToSpec(vs, streamDim)
//    val numChannels   = spec.numChannels
//    val numFrames     = spec.numFrames
//    // val afF           = File.createTemp("sysson", ".aif")
//    val afF           = java.io.File.createTempFile("sysson", ".aif", folder)
//    val af            = AudioFile.openWrite(afF, spec)
//    val fBufSize      = math.max(1, math.min(8192 / numChannels, numFrames)).toInt // file buffer
//    assert(fBufSize > 0)
//
//    logDebug(s"Audio file cache: '${afF.name}', numChannels = $numChannels, numFrames = $numFrames")
//
//    val fBuf          = af.buffer(fBufSize)
//    var framesWritten = 0L
//    val t             = if (streamDim <= 0) arr else arr.transpose(0, streamDim)
//    import at.iem.sysson.Implicits._
//    val it            = t.float1Diterator
//    while (framesWritten < numFrames) {
//      val chunk = math.min(fBufSize, numFrames - framesWritten).toInt
//      var i = 0
//      while (i < chunk) {
//        var ch = 0
//        while (ch < numChannels) {
//          fBuf(ch)(i) = it.next() // XXX TODO: would be better to have inner loop iterate for frames
//          ch += 1
//        }
//        i += 1
//      }
//      af.write(fBuf, 0, chunk)
//      framesWritten += chunk
//    }
//    af.close()
//
//    val file = vs.file.file
//    CacheValue(netSize = file.length(), netModified = file.lastModified(), data = afF, spec = spec)
//  }
//
//  def acquire[S <: Sys[S]](workspace: Workspace[S], source: DataSource.Variable[S], section: Vec[Range],
//                           streamDim: Int)
//                          (implicit tx: S#Tx, cursor: stm.Cursor[S]): Future[Result] = {
//    val key = sectionToKey(source, section, streamDim)
//    implicit val itx = tx.peer
//    map.get(key).fold {
//      val fut0 = cache.acquire(key) {
//        blocking(produceValue(workspace, source, section, streamDim))
//      }
//      val fut = fut0.map { value =>
//        Grapheme.Value.Audio(artifact = value.data, spec = value.spec, offset = 0L, gain = 1.0)
//      }
//      val e = new Entry(future = fut)
//      map.put(key, e)
//      fut.recover {
//        case NonFatal(t) =>
//          map.single.remove(key)
//          throw t
//      }
//      fut
//
//    } { e0 =>
//      val e1 = e0.inc
//      map.put(key, e1)
//      e1.future
//    }
//  }
}

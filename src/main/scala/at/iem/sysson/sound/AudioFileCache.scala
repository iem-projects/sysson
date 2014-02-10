/*
 *  AudioFileCache.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package sound

import de.sciss.filecache
import de.sciss.serial.{DataOutput, DataInput, ImmutableSerializer}
import concurrent._
import de.sciss.file._
import de.sciss.synth.proc.Grapheme
import de.sciss.synth.io.{AudioFileSpec, AudioFile}
import de.sciss.lucre.event.Sys
import scala.concurrent.stm.TMap
import at.iem.sysson.impl.Serializers

// import at.iem.sysson.Implicits._
import scala.concurrent.stm.atomic

object AudioFileCache {
  private val KEY_COOKIE  = 0x6166636B  // "afck"
  private val VAL_COOKIE  = 0x61666376  // "afcv"

  private val DEBUG   = false

  private def debug(what: => String): Unit = if (DEBUG) println(s"<cache> $what")

  type Result = Grapheme.Value.Audio

  implicit val executionContext: ExecutionContext = ExecutionContext.global

  import Serializers.RangeSerializer
  private val parentsSer  = ImmutableSerializer.list[String]
  private val sectionSer  = ImmutableSerializer.indexedSeq[Range]

  private object CacheKey {
    implicit object Serializer extends ImmutableSerializer[CacheKey] {
      def read(in: DataInput): CacheKey = {
        val cookie = in readInt()
        require(cookie == KEY_COOKIE,
          s"Unexpected cookie (expected ${KEY_COOKIE.toHexString}, found ${cookie.toHexString})")
        val path      = in readUTF()
        val parents   = parentsSer.read(in)
        val variable  = in readUTF()
        val section   = sectionSer.read(in)
        val streamDim = in readInt()
        CacheKey(file = file(path), parents = parents, variable = variable, section = section, streamDim = streamDim)
      }

      def write(v: CacheKey, out: DataOutput): Unit = {
        import v._
        out writeInt  KEY_COOKIE
        out writeUTF  file.path
        parentsSer.write(parents, out)
        out writeUTF  variable
        sectionSer.write(section, out)
        out writeInt  streamDim
      }
    }
  }
  /* Key part of the cache
   *
   * @param file      the NetCDF file
   * @param parents   the parent groups of the variable, excluding the root group
   * @param variable  the variable name
   * @param section   the sectioning of the variable
   * @param streamDim  the dimension index to stream or `-1`
   */
  private case class CacheKey(file: File, parents: List[String], variable: String, section: Vec[Range],
                              streamDim: Int) {
    // lazy val spec: AudioFileSpec = keyToSpec(this)
  }

  private def sectionToSpec(vs: VariableSection, streamDim: Int /* , key: CacheKey */): AudioFileSpec = {
    // import key._
    val isStreaming   = streamDim >= 0
    val shape         = vs.shape
    val numFrames     = if (!isStreaming) 1 else shape(streamDim)
    val size          = (0L /: shape)((sum, sz) => sum + sz)
    val numChannelsL  = size / numFrames
    require(numChannelsL <= 0xFFFF, s"The number of channels ($numChannelsL) is larger than supported")
    val numChannels   = numChannelsL.toInt
    AudioFileSpec(numFrames = numFrames, numChannels = numChannels, sampleRate = 44100 /* rate */)
  }
  
  private object CacheValue {
    implicit object Serializer extends ImmutableSerializer[CacheValue] {
      def write(v: CacheValue, out: DataOutput): Unit = {
        import v._
        out writeInt  VAL_COOKIE
        out writeLong netSize
        out writeLong netModified
        out writeUTF  data.getPath
        AudioFileSpec.Serializer.write(spec, out)
      }

      def read(in: DataInput): CacheValue = {
        val cookie = in.readInt()
        require(cookie == VAL_COOKIE, s"Serialized version $cookie does not match $VAL_COOKIE")
        val netSize       = in.readLong()
        val netModified   = in.readLong()
        val data          = new File(in.readUTF())
        val spec          = AudioFileSpec.Serializer.read(in)
        CacheValue(netSize = netSize, netModified = netModified, data = data, spec = spec)
      }
    }
  }
  private case class CacheValue(netSize: Long, netModified: Long, data: File, spec: AudioFileSpec) {
    override def toString =
      s"$productPrefix(size = $netSize, lastModified = ${new java.util.Date(netModified)}, data = ${data.getName})"
  }

  private def sectionToKey(source: DataSource.Variable[_], section: Vec[Range], streamDim: Int): CacheKey = {
    // val v = section.variable
    val f = file(source.source.path) // v.file.path

    //    val sectClosed: Vec[Range] = section.ranges.map { r =>
    //      Range.inclusive(r.first(), r.last(), r.stride())
    //    }
    CacheKey(f, source.parents, source.name, section /* sectClosed */, streamDim)
  }

  /*
    The cache is organised as follows:
    - the lookup part of the key is the NetCDF file (`get` takes a `NetcdfFile`, we actually just use its path).
    - the value completion of the key is an instance of `CacheValue`, which maintains verifiable information about
     the NetCDF file's identity (size and modification date), along with a pointer `data` to the actually
     generated stats file which is associated with the cache

    This stats file is a straight forward serialization of the `Stats` trait.

   */
  private lazy val cache = {
    val config              = filecache.Config[CacheKey, CacheValue]()
    config.capacity         = filecache.Limit(count = 500, space = 10L * 1024 * 1024 * 1024)  // 10 GB
    config.accept           = (key, value) => {
      val res = key.file.lastModified() == value.netModified && key.file.length() == value.netSize
      debug(s"accept key = ${key.file.name} (lastModified = ${new java.util.Date(key.file.lastModified())}}), value = $value? $res")
      res
    }
    config.space            = (_  , value) => value.data.length()
    config.evict            = (_  , value) => {
      debug(s"evict $value")
      value.data.delete()
    }
    config.folder           = dataDir / "cache"
    config.executionContext = AudioFileCache.executionContext
    atomic { implicit tx => filecache.TxnProducer(config) }
  }

  private val busy = TMap.empty[CacheKey, Future[Result]]

  // def acquire(section: VariableSection, streamDim: Int): Future[Result]

  private def produceValue[S <: Sys[S]](workspace: Workspace[S], source: DataSource.Variable[S],
                                        section: Vec[Range], streamDim: Int): CacheValue = {
    val v             = workspace.cursor.step { implicit tx => source.data(workspace) }
    val vs            = VariableSection(v, section.map(OpenRange.closed))
    val arr           = vs.readSafe()

    // cf. Arrays.txt for (de-)interleaving scheme
    val spec          = sectionToSpec(vs, streamDim)
    val numChannels   = spec.numChannels
    val numFrames     = spec.numFrames
    val afF           = File.createTemp("sysson", ".aif")
    val af            = AudioFile.openWrite(afF, spec)
    val fBufSize      = math.max(1, math.min(8192 / numChannels, numFrames)).toInt // file buffer
    assert(fBufSize > 0)
    val fBuf          = af.buffer(fBufSize)
    var framesWritten = 0L
    val t             = if (streamDim <= 0) arr else arr.transpose(0, streamDim)
    import at.iem.sysson.Implicits._
    val it            = t.float1Diterator
    while (framesWritten < numFrames) {
      val chunk = math.min(fBufSize, numFrames - framesWritten).toInt
      var i = 0
      while (i < chunk) {
        var ch = 0
        while (ch < numChannels) {
          fBuf(ch)(i) = it.next() // XXX TODO: would be better to have inner loop iterate for frames
          ch += 1
        }
        i += 1
      }
      af.write(fBuf, 0, chunk)
      framesWritten += chunk
    }
    af.close()

    val file = vs.file.file
    CacheValue(netSize = file.length(), netModified = file.lastModified(), data = afF, spec = spec)
  }

  def acquire[S <: Sys[S]](workspace: Workspace[S], source: DataSource.Variable[S], section: Vec[Range],
                           streamDim: Int)
                          (implicit tx: S#Tx): Future[Result] = {
    val key = sectionToKey(source, section, streamDim)
    implicit val itx = tx.peer
    busy.get(key).getOrElse {
      val fut = cache.acquire(key) {
        blocking(produceValue(workspace, source, section, streamDim))
      }

      val futM = fut.map { value =>
        Grapheme.Value.Audio(artifact = value.data, spec = value.spec, offset = 0L, gain = 1.0)
      }

      busy.put(key, futM)
      futM.onComplete { _ =>
        busy.single.remove(key)
      }
      futM
    }
  }
}

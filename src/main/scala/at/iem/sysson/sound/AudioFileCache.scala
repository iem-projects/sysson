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

import ucar.nc2
import de.sciss.filecache
import de.sciss.serial.{DataOutput, DataInput, ImmutableSerializer}
import concurrent._
import de.sciss.file._
import de.sciss.synth.proc.Grapheme
import de.sciss.synth.io.{AudioFileSpec, AudioFile}
import scala.annotation.tailrec

object AudioFileCache {
  private val KEY_COOKIE  = 0x6166636B  // "afck"
  private val VAL_COOKIE  = 0x61666376  // "afcv"

  private val DEBUG   = false

  private def debug(what: => String): Unit = if (DEBUG) println(s"<cache> $what")

  type Result = Grapheme.Value.Audio

  implicit val executionContext: ExecutionContext = ExecutionContext.global

  private implicit object RangeSerializer extends ImmutableSerializer[Range] {
    def read(in: DataInput): Range = {
      val start       = in readInt()
      val end         = in readInt()
      val isInclusive = in readBoolean()
      val step        = in readInt()
      if (isInclusive)
        Range.inclusive(start, end, step)
      else
        Range.apply    (start, end, step)
    }

    def write(r: Range, out: DataOutput): Unit = {
      import r._
      out writeInt     start
      out write        end
      out writeBoolean isInclusive
      out writeInt     step
    }
  }

  private object CacheKey {
    implicit object Serializer extends ImmutableSerializer[CacheKey] {
      def read(in: DataInput): CacheKey = {
        val cookie = in readInt()
        require(cookie == KEY_COOKIE,
          s"Unexpected cookie (expected ${KEY_COOKIE.toHexString}, found ${cookie.toHexString})")
        val path      = in readUTF()
        val parents   = ImmutableSerializer.list[String].read(in)
        val variable  = in readUTF()
        val section   = ImmutableSerializer.indexedSeq[Range].read(in)
        val streamDim = in readInt()
        CacheKey(file = file(path), parents = parents, variable = variable, section = section, streamDim = streamDim)
      }

      def write(v: CacheKey, out: DataOutput): Unit = {
        import v._
        out writeInt  KEY_COOKIE
        out writeUTF  file.path
        ImmutableSerializer.list[String]      .write(parents, out)
        out writeUTF  variable
        ImmutableSerializer.indexedSeq[Range] .write(section, out)
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
    lazy val spec: AudioFileSpec = keyToSpec(this)
  }

  private def keyToSpec(key: CacheKey): AudioFileSpec = {
    import key._
    val isStreaming   = streamDim >= 0
    val numFrames     = if (!isStreaming) 1 else section(streamDim).size
    val size          = (0L /: section)((sum, dim) => sum + dim.size)
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
        out writeLong size
        out writeLong lastModified
        out writeUTF  data.getPath
      }

      def read(in: DataInput): CacheValue = {
        val cookie = in.readInt()
        require(cookie == VAL_COOKIE, s"Serialized version $cookie does not match $VAL_COOKIE")
        val size          = in.readLong()
        val lastModified  = in.readLong()
        val data          = new File(in.readUTF())
        CacheValue(size = size, lastModified = lastModified, data = data)
      }
    }
  }
  private case class CacheValue(size: Long, lastModified: Long, data: File) {
    override def toString =
      s"$productPrefix(size = $size, lastModified = ${new java.util.Date(lastModified)}, data = ${data.getName})"
  }

  private def sectionToKey(section: VariableSection, streamDim: Int): CacheKey = {
    import at.iem.sysson.Implicits._
    val v = section.variable
    val f = file(v.file.path)

    val sectClosed: Vec[Range] = section.ranges.map { r =>
      Range.inclusive(r.first(), r.last(), r.stride())
    }
    CacheKey(f, v.parents, v.name, sectClosed, streamDim)
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
    val config              = filecache.Producer.Config[CacheKey, CacheValue]()
    config.capacity         = filecache.Limit(count = 500, space = 10L * 1024 * 1024 * 1024)  // 10 GB
    config.accept           = (key, value) => {
      val res = key.file.lastModified() == value.lastModified && key.file.length() == value.size
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
    filecache.Producer(config)
  }

  private val sync  = new AnyRef
  private var busy  = Map.empty[CacheKey, Future[Result]]

  def acquire(section: VariableSection, streamDim: Int): Future[Result] = sync.synchronized {
    val key = sectionToKey(section, streamDim)
    busy.getOrElse(key, {
      val fut = cache.acquire(key, blocking {
        val arr = section.readSafe()

        // cf. Arrays.txt for (de-)interleaving scheme

        val spec          = key.spec
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

        CacheValue(size = key.file.length(), lastModified = key.file.lastModified(), data = afF)
      })

      val futM = fut.map { value =>
        Grapheme.Value.Audio(artifact = value.data, spec = key.spec, offset = 0L, gain = 1.0)
      }

      sync.synchronized(busy += key -> futM)
      futM.onComplete(_ => sync.synchronized(busy -= key))
      futM
    })
  }
}

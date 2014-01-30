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
import collection.breakOut
import concurrent._
import Implicits._
import de.sciss.file._
import de.sciss.synth.proc.Grapheme
import de.sciss.synth.io.{AudioFileSpec, AudioFile}

object AudioFileCache {
  private val KEY_COOKIE  = 0x6166636B  // "afck"
  private val VAL_COOKIE  = 0x61666376  // "afcv"

  private val DEBUG   = false

  private def debug(what: => String): Unit = if (DEBUG) println(s"<cache> $what")

  type Result = Grapheme.Value.Audio

  private object CacheKey {
    implicit object Serializer extends ImmutableSerializer[CacheKey] {
      def read(in: DataInput): CacheKey = {
        val cookie = in readInt()
        require(cookie == KEY_COOKIE,
          s"Unexpected cookie (expected ${KEY_COOKIE.toHexString}, found ${cookie.toHexString})")
        val path      = in readUTF()
        val parents   = ImmutableSerializer.list[String].read(in)
        val variable  = in readUTF()
        val section   = ImmutableSerializer.indexedSeq[OpenRange].read(in)
        val streamDim = in readInt()
        CacheKey(file = file(path), parents = parents, variable = variable, section = section, streamDim = streamDim)
      }

      def write(v: CacheKey, out: DataOutput): Unit = {
        import v._
        out writeInt  KEY_COOKIE
        out writeUTF  file.path
        ImmutableSerializer.list[String]          .write(parents, out)
        out writeUTF  variable
        ImmutableSerializer.indexedSeq[OpenRange] .write(section, out)
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
  private case class CacheKey(
    file: File, parents: List[String], variable: String, section: Vec[OpenRange], streamDim: Int
  )
  
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
    val v     = section.variable
    val file  = v.group
    ??? // CacheKey(file, parents, v, section.section, streamDim)
  }

  /*
    The cache is organised as follows:
    - the lookup part of the key is the NetCDF file (`get` takes a `NetcdfFile`, we actually just use its path).
    - the value completion of the key is an instance of `CacheValue`, which maintains verifiable information about
     the NetCDF file's identity (size and modification date), along with a pointer `data` to the actually
     generated stats file which is associated with the cache

    This stats file is a straight forward serialisation of the `Stats` trait.

   */
  private lazy val cache = {
    val config        = filecache.Producer.Config[CacheKey, CacheValue]()
    config.capacity   = filecache.Limit(count = 500, space = 10L * 1024 * 1024 * 1024)  // 10 GB
    config.accept     = (key, value) => {
      val res = key.file.lastModified() == value.lastModified
      debug(s"accept key = ${key.file.name} (lastModified = ${new java.util.Date(key.file.lastModified())}}), value = $value? $res")
      res
    }
    config.space      = (_  , value) => value.data.length()
    config.evict      = (_  , value) => {
      debug(s"evict $value")
      value.data.delete()
    }
    config.folder     = dataDir / "cache"
    filecache.Producer(config)
  }

  implicit def executionContext = cache.executionContext

  private val sync  = new AnyRef
  private var busy  = Map.empty[CacheKey, Future[Result]]

  def acquire(section: VariableSection, streamDim: Int): Future[Result] = sync.synchronized {
    val key = sectionToKey(section, streamDim)
    busy.getOrElse(key, {
      val fut = cache.acquire(key, blocking {
        val arr = section.read()
        val n   = arr.getSize
        // if using streaming, we need probably:
        // arr.transpose()

        // cf. Arrays.txt for (de-)interleaving scheme

        val isStreaming = streamDim >= 0

        // NOT:
        // - when not streaming, we use a monophonic linearised buffer (instead of a buffer
        //   with one frame and arr.size channels).
        // ALWAYS:
        // - when streaming, we use a channel per rank-product (excluding time dimension),
        //   possibly allowing interpolations in BufRd.
        val numFrames     = /* if (!isStreaming) n else */ arr.getIndexPrivate.getShape(streamDim)
        val numChannelsL  = n / numFrames
        require(numChannelsL <= 0xFFFF, s"The number of channels ($numChannelsL) is larger than supported")

        val numChannels   = numChannelsL.toInt
        val file          = File.createTemp("sysson", ".aif")

        // WARNING: sound file should be AIFF to allow for floating point sample rates
        // (note: sample rate not used now)
        val af            = AudioFile.openWrite(file, AudioFileSpec(numChannels = numChannels, sampleRate = 44100 /* rate */))
        val fBufSize      = math.min(8192/numChannels, numFrames).toInt // file buffer
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


        ???
//        val stats: Stats = ??? // Stats(statsMap)
//        val f     = java.io.File.createTempFile("sysson", ".stats", cache.config.folder)
//        val out   = DataOutput.open(f)
//        val fSz   = key.length()
//        val fMod  = key.lastModified()
//        var succ  = false
//        try {
//          Stats.Serializer.write(stats, out)
//          out.close()
//          succ  = true
//          CacheValue(size = fSz, lastModified = fMod, data = f)
//        } finally {
//          out.close()
//          if (!succ) {
//            debug(s"Not successful. Deleting $f")
//            f.delete()
//          }
//        }
      })

      val futM = fut.map { value =>
//        blocking {
//          val in = DataInput.open(value.data)
//          try {
//            val res: Stats = ??? // Serializer.read(in)
//            // that way the caller of `get` doesn't need to bother, and all data is in RAM now
//            // TODO: if this gets too slow, because `get` is called several times, we might instead
//            // store the result in a weak hash map that calls `release` upon eviction from that weak map.
//            cache.release(key)
//            res
//          } finally {
//            in.close()
//          }
//        }
        ??? : Result
      }

      sync.synchronized(busy += key -> futM)
      futM.onComplete(_ => sync.synchronized(busy -= key))
      futM
    })
  }
}

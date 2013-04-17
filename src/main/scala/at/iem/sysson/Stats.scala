package at.iem.sysson

import ucar.nc2
import de.sciss.filecache
import java.io.File
import de.sciss.serial.{DataOutput, DataInput, ImmutableSerializer}
import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}
import concurrent._
import Implicits._

object Stats {
  private val COOKIE  = 0x53746174

  private val DEBUG   = true

  private def debug(what: => String) {
    if (DEBUG) println(s"<cache> $what")
  }

  private object CacheValue {
    implicit object Serializer extends ImmutableSerializer[CacheValue] {
      def write(v: CacheValue, out: DataOutput) {
        import v._
        out.writeInt(COOKIE)
        out.writeLong(size)
        out.writeLong(lastModified)
        out.writeUTF(data.getPath)
      }

      def read(in: DataInput): CacheValue = {
        val cookie = in.readInt()
        require(cookie == COOKIE, s"Serialized version $cookie does not match $COOKIE")
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

  /*
    The cache is organised as follows:
    - the lookup part of the key is the NetCDF file (`get` takes a `NetcdfFile`, we actually just use its path).
    - the value completion of the key is an instance of `CacheValue`, which maintains verifiable information about
     the NetCDF file's identity (size and modification date), along with a pointer `data` to the actually
     generated stats file which is associated with the cache

    This stats file is a straight forward serialisation of the `Stats` trait.

   */

  private lazy val cache = {
    val config        = filecache.Producer.Config[File, CacheValue]()
    config.capacity   = filecache.Limit(count = 100, space = 1L * 1024 * 1024)  // 1 MB... whatever, these files are incredibly small
    config.accept     = (key, value) => {
      val res = key.lastModified() == value.lastModified && key.length() == value.size
      debug(s"accept key = ${key.getName} (lastModified = ${new java.util.Date(key.lastModified())}, size = ${key.length()}), value = $value? $res")
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

  def get(doc: nc2.NetcdfFile): Future[Stats] = {
    val key = file(doc.path)
    val fut = cache.acquire(key, blocking {
      // for each variable...
      val varMap    = doc.variableMap
      val statsMap  = varMap.collect { case (vrName, vr) if vr.isFloat || vr.isDouble =>
        // find the named dimensions which have variable entries.
        val dims0: Set[nc2.Variable] = vr.reducedDimensions.flatMap(d => varMap.get(d.name.getOrElse("??")))(breakOut)
        // exclude the variable's self dimension. this
        // is present in all variables which acts as dimensions.
        // for example `plev` will have itself as its only dimension.
        val dims = dims0 - vr
        // then build the Map[String, IIdxSeq[Counts]] by iteratively reducing all dimensions but one, over which
        // the stats are generated

        val accept = if (vr.isFloat) {
          val f = vr.fillValue
          if (f.isNaN) !(_: Double).isNaN else (_: Double).toFloat != f
        } else {
          (_: Double) => true  // no fill values for non-float vars, accept all
        }

        def sectionCounts(sel: VariableSection): PreCounts = {
          val arr = sel.read()
          // val num = arr.size
          var sum = 0.0
          var min = Double.MaxValue
          var max = Double.MinValue
          var num = 0
          arr.double1Diterator.foreach { d =>
            if (accept(d)) {
              num += 1
              sum += d
              if (min > d) min = d
              if (max < d) max = d
            }
          }
          val mean    = sum/num
          var sqrDif  = 0.0
          arr.double1Diterator.foreach { d =>
            if (accept(d)) {
              val dif = d - mean
              sqrDif += dif * dif
            }
          }
          // val stddev = math.sqrt(sqrDif/num)
          PreCounts(num = num, min = min, max = max, sum = sum, sqrDif = sqrDif)
        }

        val preSlices: Map[String, IIdxSeq[PreCounts]] = dims.map(dim => {
          // val redDims = dims - dim
          val counts  = Vector.tabulate(dim.size.toInt) { i =>
            val sel = vr in dim.name select i
            sectionCounts(sel)
          }
          dim.name -> counts
        })(breakOut)

        val total = if (preSlices.isEmpty) { // run the iteration code again if the variable is 1-dimensional
          sectionCounts(vr.selectAll).complete
        } else {                            // otherwise reconstruct total counts from partial counts
          preSlices.head._2.reduce(_ combineWith _).complete
        }
        val statsVar = Variable(vrName, total, preSlices.mapValues(_.map(_.complete)))
        vrName -> statsVar
      }
      val stats = Stats(statsMap)
      val f     = File.createTempFile("sysson", ".stats", cache.config.folder)
      val out   = DataOutput.open(f)
      val fSz   = key.length()
      val fMod  = key.lastModified()
      var succ  = false
      try {
        Stats.Serializer.write(stats, out)
        out.close()
        succ  = true
        CacheValue(size = fSz, lastModified = fMod, data = f)
      } finally {
        out.close()
        if (!succ) {
          debug(s"Not successful. Deleting $f")
          f.delete()
        }
      }
    })
    import cache.executionContext
    fut.map { value =>
      blocking {
        val in = DataInput.open(value.data)
        try {
          val res = Serializer.read(in)
          // that way the caller of `get` doesn't need to bother, and all data is in RAM now
          // TODO: if this gets too slow, because `get` is called several times, we might instead
          // store the result in a weak hash map that calls `release` upon eviction from that weak map.
          cache.release(key)
          res
        } finally {
          in.close()
        }
      }
    }
  }

  object Counts {
    implicit object Serializer extends ImmutableSerializer[Counts] {
      def write(v: Counts, out: DataOutput) {
        import v._
        out.writeDouble(min)
        out.writeDouble(max)
        out.writeDouble(mean)
        out.writeDouble(stddev)
      }

      def read(in: DataInput): Counts = {
        val min     = in.readDouble()
        val max     = in.readDouble()
        val mean    = in.readDouble()
        val stddev  = in.readDouble()
        Counts(min = min, max = max, mean = mean, stddev = stddev)
      }
    }
  }
  case class Counts(min: Double, max: Double, mean: Double, stddev: Double) {
    override def toString = s"$productPrefix(min = ${min.toFloat}, max = ${max.toFloat}, mean = ${mean.toFloat}, stddev = ${stddev.toFloat})"
  }

  private case class PreCounts(num: Long, min: Double, max: Double, sum: Double, sqrDif: Double) {
    def complete = Counts(min = min, max = max, mean = sum/num, stddev = math.sqrt(sqrDif/num))
    def combineWith(that: PreCounts) = PreCounts(
      num     = this.num + that.num,
      min     = math.min(this.min, that.min),
      max     = math.max(this.max, that.max),
      sum     = this.sum + that.sum,
      sqrDif  = this.sqrDif + that.sqrDif
    )
  }

  object Variable {
    implicit object Serializer extends ImmutableSerializer[Variable] {
      def write(v: Variable, out: DataOutput) {
        import v._
        out.writeUTF(name)
        Counts.Serializer.write(total, out)
        ImmutableSerializer.map[String, IIdxSeq[Counts]].write(slices, out)
      }

      def read(in: DataInput): Variable = {
        val name    = in.readUTF()
        val total   = Counts.Serializer.read(in)
        val slices  = ImmutableSerializer.map[String, IIdxSeq[Counts]].read(in)
        Variable(name, total, slices)
      }
    }
  }
  case class Variable(name: String, total: Counts, slices: Map[String, IIdxSeq[Counts]]) {
    override def toString = s"(\"$name\", total = $total${if (slices.isEmpty) "" else slices.keys.mkString(", slices = <", ",", ">")})"
  }

  implicit object Serializer extends ImmutableSerializer[Stats] {
    def write(v: Stats, out: DataOutput) {
      import v._
      out.writeInt(COOKIE)
      ImmutableSerializer.map[String, Variable].write(map, out)
    }

    def read(in: DataInput): Stats = {
      val cookie = in.readInt()
      require(cookie == COOKIE, s"Serialized version $cookie does not match $COOKIE")
      val map = ImmutableSerializer.map[String, Variable].read(in)
      Stats(map)
    }
  }
}

/** Statistics for a given file. It provides a map from variable names to `Stats.Variable` which captures
  * the moments of that particular variable.
  *
  * Statistics are obtained via `Stats.get(netcdfFile)`.
  *
  * For example, if the file has a variable `"ta"` with dimensions `"lon"`, `"lat"` and `"plev"`, the
  * overall minimum temperature will be found through `stats("ta").total.min`. That maximum temperature
  * at pressure level 0 will be found through `stats("ta").slices("plev")(0).max`.
  */
case class Stats(map: Map[String, Stats.Variable]) {
  def apply(name: String) = map(name)

  override def toString = {
    val sb = new java.lang.StringBuilder(128)
    sb.append(productPrefix)
    sb.append('(')
    map.values.foreach { v =>
      sb.append("\n  ")
      sb.append(v)
    }
    if (map.nonEmpty) sb.append('\n')
    sb.append(')')
    sb.toString
  }
}
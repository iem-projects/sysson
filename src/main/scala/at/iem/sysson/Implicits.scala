package at.iem.sysson

import ucar.{nc2, ma2}
import collection.JavaConversions
import de.sciss.synth
import synth.ugen
import collection.immutable.{IndexedSeq => IIdxSeq}
import java.io.File

object Implicits {
  final val all = OpenRange.all
//  final val at  = OpenRange.at

  object end    // used in expressions such as 1 to end
  object start {
    def to   (idx: Int):    OpenRange = OpenRange(startOption = None, endOption = Some(idx), isInclusive = true)
    def to   (e: end.type): OpenRange = OpenRange.all
    def until(idx: Int):    OpenRange = OpenRange(startOption = None, endOption = Some(idx), isInclusive = false)
    def until(e: end.type): OpenRange = OpenRange.all
  }

//  implicit def sectionAll(v: nc2.Variable): VariableSection = {
//    val s = IIdxSeq.fill(v.rank)(all) // new ma2.Section(v.getShape)
//    new VariableSection(v, s)
//  }

  // important: use name distinct from `RichInt` which would otherwise shadow enrichments from ScalaCollider
  implicit class SyRichInt(i: Int) {
    def to   (e: end.type): OpenRange = OpenRange(startOption = Some(i), endOption = None, isInclusive = true)
    def until(e: end.type): OpenRange = OpenRange(startOption = Some(i), endOption = None, isInclusive = false)

    def isPowerOfTwo: Boolean = (i & (i-1)) == 0
    def nextPowerOfTwo: Int = {
   		var j = 1
   		while( j < i ) j <<= 1
   		j
   	}
  }

  implicit class SyRichNetcdfFile(peer: nc2.NetcdfFile)
    extends impl.HasDimensions with impl.HasAttributes with impl.HasVariables {

    import JavaConversions._
    def path          = peer.getLocation
    def dimensions    = peer.getDimensions.toIndexedSeq
    def attributes    = peer.getGlobalAttributes.toIndexedSeq
    def rootGroup     = peer.getRootGroup
    def variables     = peer.getVariables.toIndexedSeq

    def exportAsCSV(file: File, delimiter: Char = ',') { util.Export.netcdfToCSV(file, peer, delimiter) }
  }

  implicit class SyRichAttribute(peer: nc2.Attribute) {
    def name          = peer.getName
    def dataType      = peer.getDataType
    def size          = peer.getLength
    def values        = peer.getValues
  }

  implicit class SyRichGroup(peer: nc2.Group) extends impl.HasDimensions with impl.HasAttributes {
    import JavaConversions._
    def name          = peer.getName
    def attributes    = peer.getAttributes.toIndexedSeq
    def dimensions    = peer.getDimensions.toIndexedSeq
    def variables     = peer.getVariables.toIndexedSeq
    def children      = peer.getGroups.toIndexedSeq
    def parent        = Option(peer.getParentGroup)
  }

  implicit class SyRichDimension(peer: nc2.Dimension) {
    def name          = Option(peer.getName)
    def size          = peer.getLength
    def group         = Option(peer.getGroup)
  }

  implicit class SyRichVariable(peer: nc2.Variable)
    extends impl.HasDimensions with impl.HasAttributes with impl.VariableLike {

    import JavaConversions._
    def fullName      = peer.getFullName
    def name          = peer.getShortName
    def dataType      = peer.getDataType
    def shape         = peer.getShape.toIndexedSeq
    /** Reports the total number of elements within the variable's matrix */
    def size          = peer.getSize
    def rank          = peer.getRank
    def attributes    = peer.getAttributes.toIndexedSeq
    def description   = Option(peer.getDescription)
    def group         = Option(peer.getParentGroup)
    def dimensions    = peer.getDimensions.toIndexedSeq
    def ranges        = peer.getRanges.toIndexedSeq

    def file: nc2.NetcdfFile = {
      val field = classOf[nc2.Variable].getDeclaredField("ncfile")
      field.setAccessible(true)
      field.get(peer).asInstanceOf[nc2.NetcdfFile]
    }

    def units         = Option(peer.getUnitsString)
    def isFloat       = dataType == ma2.DataType.FLOAT
    def isDouble      = dataType == ma2.DataType.DOUBLE
    def fillValue: Float = {
      require(isFloat, s"fillValue only defined for floating point variables ($dataType)")
      attributeMap.get("_FillValue").map(_.getNumericValue.floatValue()).getOrElse(Float.NaN)
    }

    // it would be good to shadow peer.read(), but because it takes precedence over
    // an equally named enriched method, there is no way to enforce this. use `readSafe` instead.
    def readSafe()    = peer.synchronized(peer.read())

    def in(dim: String): VariableSection.In = selectAll.in(dim)

    protected def scale = Scale.Identity

    def selectAll: VariableSection = {
      val s = IIdxSeq.fill(rank)(all)
      new VariableSection(peer, s)
    }

    def applyScale(scale: Scale) = selectAll.applyScale(scale)
  }

  private def checkNaNFun(fillValue: Float): Float => Boolean = if (java.lang.Float.isNaN(fillValue))
    java.lang.Float.isNaN _
  else
    _ == fillValue

  implicit class SyRichArray(peer: ma2.Array) {
    def size          = peer.getSize
    def elementType   = peer.getElementType
    def rank          = peer.getRank
    def shape         = peer.getShape.toIndexedSeq
//    def f1d_force: IndexedSeq[Float] = float1d(force = true)
//    def f1d: IndexedSeq[Float] = float1d(force = true)

    def isFloat       = peer.getElementType == classOf[Float]
    def isDouble      = peer.getElementType == classOf[Double]

    private def requireFloat() {
      require(isFloat, s"Wrong element type (${peer.getElementType}); required: Float")
    }

    private def requireDouble() {
      require(isDouble, s"Wrong element type (${peer.getElementType}); required: Double")
    }

    def scaled1D(scale: Scale): IIdxSeq[Float] = {
      val it = float1DIter
      val sz = peer.getSize
      Vector.fill(sz.toInt)(scale(it.getFloatNext).toFloat)
    }

    def float1D: IIdxSeq[Float] = {
      val it = float1DIter
      val sz = peer.getSize
      Vector.fill(sz.toInt)(it.getFloatNext)
    }

    def double1D: IIdxSeq[Double] = {
      val it = double1DIter
      val sz = peer.getSize
      Vector.fill(sz.toInt)(it.getDoubleNext)
    }

    def float1Diterator: Iterator[Float] = {
      val it = peer.getIndexIterator
      if (isFloat)
        new Iterator[Float] {
          def hasNext = it.hasNext
          def next()  = it.getFloatNext
        }
      else if (isDouble)
        new Iterator[Float] {
          def hasNext = it.hasNext
          def next()  = it.getDoubleNext.toFloat
        }
      else sys.error(s"Data type is neither Float nor Double")
    }

    def double1Diterator: Iterator[Double] = {
      val it = peer.getIndexIterator
      if (isFloat)
        new Iterator[Double] {
          def hasNext = it.hasNext
          def next()  = it.getFloatNext.toDouble
        }
      else if (isDouble)
        new Iterator[Double] {
          def hasNext = it.hasNext
          def next()  = it.getDoubleNext
        }
      else sys.error(s"Data type is neither Float nor Double")
    }

    private def float1DIter: ma2.IndexIterator = {
      requireFloat()
//      if (!force) require(peer.getRank == 1, s"Wrong rank (${peer.getRank}); required: 1")
      val sz = peer.getSize
      require(sz <= 0x7FFFFFFF, s"Array too large (size = $sz)")
      peer.getIndexIterator
    }

    private def double1DIter: ma2.IndexIterator = {
      requireDouble()
//      if (!force) require(peer.getRank == 1, s"Wrong rank (${peer.getRank}); required: 1")
      val sz = peer.getSize
      require(sz <= 0x7FFFFFFF, s"Array too large (size = $sz)")
      peer.getIndexIterator
    }

    def minmax: (Double, Double) = minmax(Float.NaN)
    def minmax(fillValue: Float): (Double, Double) = {
      requireFloat()
      val checkNaN = checkNaNFun(fillValue)
      val it  = peer.getIndexIterator
      var min = Float.MaxValue
      var max = Float.MinValue
      while (it.hasNext) {
        val f = it.getFloatNext
        if (!checkNaN(f)) {
          if (f < min) min = f
          if (f > max) max = f
        }
      }
      (min, max)
    }
  }

  implicit class SyRichFloatSeq(peer: IIdxSeq[Float]) {
    def replaceNaNs(value: Float, fillValue: Float = Float.NaN): IIdxSeq[Float] = {
      val checkNaN = checkNaNFun(fillValue)
      peer.map { f =>
        if (checkNaN(f)) value else f
      }
    }

    def dropNaNs: IIdxSeq[Float] = dropNaNs(Float.NaN)
    def dropNaNs(fillValue: Float): IIdxSeq[Float] = {
      val checkNaN = checkNaNFun(fillValue)
      peer.filterNot(checkNaN)
    }

    def normalize: IIdxSeq[Float] = normalize(Float.NaN)
    def normalize(fillValue: Float): IIdxSeq[Float] = {
      val sz   = peer.size
      if( sz == 0 ) return peer
      var min  = Float.MaxValue
      var max  = Float.MinValue

      val checkNaN = checkNaNFun(fillValue)

      var i = 0; while (i < sz) {
        val f = peer(i)
        if(java.lang.Float.isInfinite(f)) sys.error("Unbounded value: " + f)

        if (!checkNaN(f)) {
          if (f < min) min = f
          if (f > max) max = f
        }
      i += 1 }
      val div = max - min
      if (div == 0f) return Vector.fill(sz)(0f)
      assert(div > 0f)
      val mul = 1f / div
      peer.map(f => if (checkNaN(f)) f else (f - min) * mul)
    }

    def linlin(srcLo: Double = 0.0, srcHi: Double = 1.0, fillValue: Float = Float.NaN)(tgtLo: Double, tgtHi: Double): IIdxSeq[Float] = {
      require(srcLo != srcHi, "Source range is zero (lo = " + srcLo + ", hi = " + srcHi + ")")
      require(tgtLo != tgtHi, "Target range is zero (lo = " + tgtLo + ", hi = " + tgtHi + ")")
      val checkNaN = checkNaNFun(fillValue)
      val add1 = -srcLo
      val mul  = (tgtHi - tgtLo) / (srcHi - srcLo)
      val add2 = tgtLo
      peer.map(f => if (checkNaN(f)) f else ((f + add1) * mul + add2).toFloat)
    }

    def asEnv(dur: Double, shape: synth.Env.ConstShape = synth.stepShape): ugen.EnvGen = {
      import synth._
         import ugen._
      val sz = peer.size
      require(sz > 0, "Sequence is empty")
      val segDur  = dur / sz
      val env = Env(peer.head, peer.tail.map(mag => Env.Seg(segDur, mag, shape)) :+ Env.Seg(segDur, peer.last, stepShape))
      EnvGen.ar(env, doneAction = freeSelf)
    }
  }

  implicit class SyRichFile(f: File) {
    def /(child: String): File = new File(f, child)
    def path: String  = f.getPath
    def name: String  = f.getName
    def parent: File  = f.getParentFile
  }
}
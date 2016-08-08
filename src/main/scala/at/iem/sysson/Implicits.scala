/*
 *  Implicits.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2016 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson

import ucar.{nc2, ma2}
import collection.JavaConversions
import de.sciss.synth
import synth.ugen
import java.io.File
import scala.concurrent.{Await, Future}
import scala.annotation.tailrec
import scala.collection.breakOut

object Implicits {
  final val all = OpenRange.all

  object end    // used in expressions such as 1 to end
  object start {
    def to   (idx: Int):    OpenRange = OpenRange(startOption = None, endOption = Some(idx), isInclusive = true)
    def to   (e: end.type): OpenRange = OpenRange.all
    def until(idx: Int):    OpenRange = OpenRange(startOption = None, endOption = Some(idx), isInclusive = false)
    def until(e: end.type): OpenRange = OpenRange.all
  }

  // important: use name distinct from `RichInt` which would otherwise shadow enrichments from ScalaCollider
  implicit class SyRichInt(val toInt: Int) extends AnyVal {
    def to   (e: end.type): OpenRange = OpenRange(startOption = Some(toInt), endOption = None, isInclusive = true )
    def until(e: end.type): OpenRange = OpenRange(startOption = Some(toInt), endOption = None, isInclusive = false)

    def isPowerOfTwo: Boolean = (toInt & (toInt-1)) == 0
    def nextPowerOfTwo: Int = {
   		var j = 1
      while (j < toInt) j <<= 1
   		j
   	}
  }

  implicit class SyRichNetcdfFile(val peer: nc2.NetcdfFile)
    extends AnyVal with impl.HasDimensions with impl.HasAttributes with impl.HasVariables {

    import JavaConversions._
    def path       : String             = peer.getLocation
    def dimensions : Vec[nc2.Dimension] = peer.getDimensions.toIndexedSeq
    def attributes : Vec[nc2.Attribute] = peer.getGlobalAttributes.toIndexedSeq
    def rootGroup  : nc2.Group          = peer.getRootGroup
    def variables  : Vec[nc2.Variable]  = peer.getVariables.toIndexedSeq
    def file       : File               = new File(path)

    def exportAsCSV(file: File, delimiter: Char = ','): Unit = util.Export.netcdfToCSV(file, peer, delimiter)
  }

  implicit class SyRichAttribute(val peer: nc2.Attribute) extends AnyVal {
    def name       : String             = peer.getShortName // getName
    def dataType   : ma2.DataType       = peer.getDataType
    def size       : Int                = peer.getLength
    def values     : ma2.Array          = peer.getValues
  }

  implicit class SyRichGroup(val peer: nc2.Group) extends AnyVal with impl.HasDimensions with impl.HasAttributes {
    import JavaConversions._
    def name       : String             = peer.getFullName  // getName
    def attributes : Vec[nc2.Attribute] = peer.getAttributes.toIndexedSeq
    def dimensions : Vec[nc2.Dimension] = peer.getDimensions.toIndexedSeq
    def variables  : Vec[nc2.Variable]  = peer.getVariables.toIndexedSeq
    def children   : Vec[nc2.Group]     = peer.getGroups.toIndexedSeq
    def parentOption: Option[nc2.Group] = Option(peer.getParentGroup)
  }

  implicit class SyRichDimension(val peer: nc2.Dimension) extends AnyVal {
    def name       : String             = nameOption.getOrElse("?")
    def nameOption : Option[String]     = Option(peer.getShortName) // getName
    def size       : Int                = peer.getLength
    def groupOption: Option[nc2.Group]  = Option(peer.getGroup)
  }

  @tailrec private def parentsLoop(g: nc2.Group, res: List[String]): List[String] = g.parentOption match {
    case Some(p)  => parentsLoop(p, g.name :: res)
    case _        => res
  }

  implicit class SyRichVariable(val peer: nc2.Variable)
    extends AnyVal with impl.HasDimensions with impl.HasAttributes with impl.VariableLike {

    import JavaConversions._
    def fullName   : String             = peer.getFullName
    def name       : String             = peer.getShortName
    def dataType   : ma2.DataType       = peer.getDataType
    def shape      : Vec[Int]           = peer.getShape.toIndexedSeq
    /** Reports the total number of elements within the variable's matrix */
    def size       : Long               = peer.getSize
    def rank       : Int                = peer.getRank
    def attributes : Vec[nc2.Attribute] = peer.getAttributes.toIndexedSeq
    def description: Option[String]     = Option(peer.getDescription)
    def group      : nc2.Group          = peer.getParentGroup
    def dimensions : Vec[nc2.Dimension] = peer.getDimensions.toIndexedSeq
    def ranges     : Vec[Range]         = peer.getRanges.map {
      ma => Range.inclusive(ma.first(), ma.last(), ma.stride())
    } (breakOut)

    def file       : nc2.NetcdfFile     = peer.getParentGroup.getNetcdfFile

    /** List of group names which lead from root (excluded) to this variable. */
    def parents    : List[String]       = parentsLoop(peer.group, Nil)

    def units      : Option[String]     = Option(peer.getUnitsString)
    def isFloat    : Boolean            = dataType == ma2.DataType.FLOAT
    def isDouble   : Boolean            = dataType == ma2.DataType.DOUBLE
    def isInt      : Boolean            = dataType == ma2.DataType.INT

    def fillValue  : Double = {
      if (!isFloat && !isDouble) sys.error(s"fillValue only defined for floating point variables ($dataType)")
      attributeMap.get("_FillValue").map(_.getNumericValue.doubleValue()).getOrElse(Double.NaN)
    }

    // it would be good to shadow peer.read(), but because it takes precedence over
    // an equally named enriched method, there is no way to enforce this. use `readSafe` instead.
    def readSafe() : ma2.Array          = file.synchronized(peer.read())

    def in(dim: String): VariableSection.In = selectAll.in(dim)

    protected def scale: Scale = Scale.Identity

    def selectAll: VariableSection = {
      val s = Vec.fill(rank)(all)
      new VariableSection(peer, s)
    }

    def applyScale(scale: Scale): VariableSection = selectAll.applyScale(scale)
  }

  private def checkNaNFun(fillValue: Float): Float => Boolean = if (java.lang.Float.isNaN(fillValue))
    java.lang.Float.isNaN
  else
    _ == fillValue

  private def checkNaNFun(fillValue: Double): Double => Boolean = if (java.lang.Double.isNaN(fillValue))
    java.lang.Double.isNaN
  else
    _ == fillValue

  implicit class SyRichArray(val peer: ma2.Array) extends AnyVal {
    def size       : Long               = peer.getSize
    def elementType: Class[_]           = peer.getElementType
    def rank       : Int                = peer.getRank
    def shape      : Vec[Int]           = peer.getShape.toIndexedSeq

    //    def f1d_force: IndexedSeq[Float] = float1d(force = true)
    //    def f1d: IndexedSeq[Float] = float1d(force = true)

    def isFloat    : Boolean            = peer.getElementType == classOf[Float ]
    def isDouble   : Boolean            = peer.getElementType == classOf[Double]
    def isInt      : Boolean            = peer.getElementType == classOf[Int   ]

    private def requireFloat(): Unit =
      require(isFloat, s"Wrong element type (${peer.getElementType}); required: Float")

//    private def requireDouble(): Unit =
//      require(isDouble, s"Wrong element type (${peer.getElementType}); required: Double")

    private def requireInt(): Unit =
      require(isInt, s"Wrong element type (${peer.getElementType}); required: Int")

    def scaled1D(scale: Scale): Vec[Float] = {
      val it = float1DIter
      val sz = peer.getSize
      Vector.fill(sz.toInt)(scale(it.getFloatNext).toFloat)
    }

    def float1D: Vec[Float] = {
      // val it = float1DIter
      // val sz = peer.getSize
      // Vector.fill(sz.toInt)(it.getFloatNext)
      float1Diterator.toVector
    }

    def double1D: Vec[Double] = {
      // val it = double1DIter
      // val sz = peer.getSize
      // Vector.fill(sz.toInt)(it.getDoubleNext)
      double1Diterator.toVector
    }

    def int1D: Vec[Int] = {
      val it = int1DIter
      val sz = peer.getSize
      Vector.fill(sz.toInt)(it.getIntNext)
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

//    private def double1DIter: ma2.IndexIterator = {
//      requireDouble()
//      //      if (!force) require(peer.getRank == 1, s"Wrong rank (${peer.getRank}); required: 1")
//      val sz = peer.getSize
//      require(sz <= 0x7FFFFFFF, s"Array too large (size = $sz)")
//      peer.getIndexIterator
//    }

    private def int1DIter: ma2.IndexIterator = {
      requireInt()
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

  implicit class SyRichFloatSeq(val peer: Vec[Float]) extends AnyVal {
    def replaceNaNs(value: Float, fillValue: Float = Float.NaN): Vec[Float] = {
      val checkNaN = checkNaNFun(fillValue)
      peer.map { f =>
        if (checkNaN(f)) value else f
      }
    }

    def dropNaNs: Vec[Float] = dropNaNs(Float.NaN)
    def dropNaNs(fillValue: Float): Vec[Float] = {
      val checkNaN = checkNaNFun(fillValue)
      peer.filterNot(checkNaN)
    }

    def normalize: Vec[Float] = normalize(Float.NaN)
    def normalize(fillValue: Float): Vec[Float] = {
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
      if (div <= 0f) return Vector.fill(sz)(0f)
      // assert(div > 0f, s"max = $max, min = $min, div = $div")
      val mul = 1f / div
      peer.map(f => if (checkNaN(f)) f else (f - min) * mul)
    }

    def linlin(srcLo: Double = 0.0, srcHi: Double = 1.0, fillValue: Float = Float.NaN)(tgtLo: Double, tgtHi: Double): Vec[Float] = {
      require(srcLo != srcHi, "Source range is zero (lo = " + srcLo + ", hi = " + srcHi + ")")
      require(tgtLo != tgtHi, "Target range is zero (lo = " + tgtLo + ", hi = " + tgtHi + ")")
      val checkNaN = checkNaNFun(fillValue)
      val add1 = -srcLo
      val mul  = (tgtHi - tgtLo) / (srcHi - srcLo)
      val add2 = tgtLo
      peer.map(f => if (checkNaN(f)) f else ((f + add1) * mul + add2).toFloat)
    }

    def asEnv(dur: Double, shape: synth.Curve = synth.Curve.step): ugen.EnvGen = {
      import synth._
      import ugen._
      val sz = peer.size
      require(sz > 0, "Sequence is empty")
      val segDur  = dur / sz
      val env = Env(peer.head, peer.tail.map(mag =>
        Env.Segment(segDur, mag, shape)) :+ Env.Segment(segDur, peer.last, Curve.step)
      )
      EnvGen.ar(env, doneAction = freeSelf)
    }
  }

  implicit class SyRichDoubleSeq(val peer: Vec[Double]) extends AnyVal {
    def replaceNaNs(value: Double, fillValue: Double = Double.NaN): Vec[Double] = {
      val checkNaN = checkNaNFun(fillValue)
      peer.map { f =>
        if (checkNaN(f)) value else f
      }
    }

    def dropNaNs: Vec[Double] = dropNaNs(Double.NaN)
    def dropNaNs(fillValue: Double): Vec[Double] = {
      val checkNaN = checkNaNFun(fillValue)
      peer.filterNot(checkNaN)
    }

    def normalize: Vec[Double] = normalize(Double.NaN)
    def normalize(fillValue: Double): Vec[Double] = {
      val sz   = peer.size
      if( sz == 0 ) return peer
      var min  = Double.MaxValue
      var max  = Double.MinValue

      val checkNaN = checkNaNFun(fillValue)

      var i = 0; while (i < sz) {
        val f = peer(i)
        if(java.lang.Double.isInfinite(f)) sys.error("Unbounded value: " + f)

        if (!checkNaN(f)) {
          if (f < min) min = f
          if (f > max) max = f
        }
        i += 1 }
      val div = max - min
      if (div <= 0.0) return Vector.fill(sz)(0.0)
      // assert(div > 0f, s"max = $max, min = $min, div = $div")
      val mul = 1.0 / div
      peer.map(f => if (checkNaN(f)) f else (f - min) * mul)
    }

    def linlin(srcLo: Double = 0.0, srcHi: Double = 1.0, fillValue: Double = Double.NaN)(tgtLo: Double, tgtHi: Double): Vec[Double] = {
      require(srcLo != srcHi, "Source range is zero (lo = " + srcLo + ", hi = " + srcHi + ")")
      require(tgtLo != tgtHi, "Target range is zero (lo = " + tgtLo + ", hi = " + tgtHi + ")")
      val checkNaN = checkNaNFun(fillValue)
      val add1 = -srcLo
      val mul  = (tgtHi - tgtLo) / (srcHi - srcLo)
      val add2 = tgtLo
      peer.map(f => if (checkNaN(f)) f else (f + add1) * mul + add2)
    }
  }

  implicit class SyRichFuture[A](val peer: Future[A]) extends AnyVal {
    def !! : A = {
      import concurrent.duration._
      Await.result(peer, 3.seconds)
    }
  }
}
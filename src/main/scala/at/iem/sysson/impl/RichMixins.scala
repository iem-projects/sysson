package at.iem.sysson
package impl

import ucar.{nc2, ma2}
import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}

trait HasDimensions {
  import Implicits._
  def dimensions: IIdxSeq[nc2.Dimension]
  def dimensionMap: Map[String, nc2.Dimension] = dimensions.map(d => d.name -> d)
    .collect({ case (Some(name), d) => name -> d })(breakOut)
}

trait HasAttributes {
  import Implicits._
  def attributes: IIdxSeq[nc2.Attribute]
  def attributeMap: Map[String, nc2.Attribute] = attributes.map(a => a.name -> a)(breakOut)
}

trait HasVariables {
  import Implicits._
  def variables: IIdxSeq[nc2.Variable]
  def variableMap: Map[String, nc2.Variable] = variables.map(a => a.name -> a)(breakOut)
}

trait VariableLike extends HasDimensions {
  def name: String
  def dataType: ma2.DataType
  def shape: IIdxSeq[Int]

  def size: Long
  def rank: Int
  def ranges: IIdxSeq[ma2.Range]

  def read(): ma2.Array

  private def effectiveDimIndices: IIdxSeq[Int] = {
    val sh  = shape
    (0 until rank).filter(sh(_) > 1)
  }

  def reducedRank   = shape.count(_ > 1)
  def reducedShape  = shape.filter(_ > 1)
  def reducedDimensions: IIdxSeq[nc2.Dimension] = {
    val dim = dimensions
    effectiveDimIndices.map(dim.apply)
  }
  def reducedRanges: IIdxSeq[ma2.Range] = {
    val r = ranges
    effectiveDimIndices.map(r.apply)
  }

  def in(dim: String): VariableSection.In

  def selectAll: VariableSection

  // ---- data manipulation ----

  def min: Double = minmax._1
  def max: Double = minmax._2

  def minmax: (Double, Double) = {
    import Implicits._
    read().minmax
  }

  def normalized: VariableSection = {
    val (min, max) = minmax
    applyScale(Scale.LinLin(srcLo = min, srcHi = max, dstLo = 0.0, dstHi = 1.0))
  }

  protected def scale: Scale

  def linlin(srcLo: Double, srcHi: Double, dstLo: Double, dstHi: Double, clip: Boolean = false): VariableSection = {
    require(scale == Scale.Identity, "Cannot change scales")
    applyScale(Scale.LinLin(srcLo = srcLo, srcHi = srcHi, dstLo = dstLo, dstHi = dstHi, clip = clip))
  }

  def linexp(srcLo: Double, srcHi: Double, dstLo: Double, dstHi: Double, clip: Boolean = false): VariableSection = {
    require(scale == Scale.Identity, "Cannot change scales")
    applyScale(Scale.LinExp(srcLo = srcLo, srcHi = srcHi, dstLo = dstLo, dstHi = dstHi, clip = clip))
  }

  def applyScale(scale: Scale): VariableSection
  def resetScale: VariableSection = applyScale(Scale.Identity)
}
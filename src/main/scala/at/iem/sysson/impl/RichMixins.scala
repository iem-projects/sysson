/*
 *  RichMixins.scala
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
package impl

import ucar.{nc2, ma2}
import collection.breakOut

trait HasDimensions extends Any {
  import Implicits._
  def dimensions: Vec[nc2.Dimension]
  def dimensionMap: Map[String, nc2.Dimension] = dimensions.map(d => d.nameOption -> d)
    .collect({ case (Some(name), d) => name -> d })(breakOut)
}

trait HasAttributes extends Any {
  import Implicits._
  def attributes: Vec[nc2.Attribute]
  def attributeMap: Map[String, nc2.Attribute] = attributes.map(a => a.name -> a)(breakOut)
}

trait HasVariables extends Any {
  import Implicits._
  def variables: Vec[nc2.Variable]
  def variableMap: Map[String, nc2.Variable] = variables.map(a => a.name -> a)(breakOut)
}

trait VariableLike extends Any with HasDimensions {
  def name: String
  def dataType: ma2.DataType
  def shape: Vec[Int]

  def size: Long
  def rank: Int
  def ranges: Vec[Range]

  def read    (): ma2.Array = readSafe()
  def readSafe(): ma2.Array

  // this requires reflection because stupidly there is no public accessor on nc2.Variable
  def file: nc2.NetcdfFile

  private def effectiveDimIndices: Vec[Int] = {
    val sh  = shape
    (0 until rank).filter(sh(_) > 1)
  }

  def reducedRank : Int       = shape.count (_ > 1)
  def reducedShape: Vec[Int]  = shape.filter(_ > 1)

  def reducedDimensions: Vec[nc2.Dimension] = {
    val dim = dimensions
    effectiveDimIndices.map(dim.apply)
  }

  def reducedRanges: Vec[Range] = {
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
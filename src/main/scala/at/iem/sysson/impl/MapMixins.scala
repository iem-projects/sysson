package at.iem.sysson
package impl

import ucar.nc2
import collection.breakOut

trait HasDimensions {
  import Implicits._
  def dimensions: IndexedSeq[nc2.Dimension]
  def dimensionMap: Map[String, nc2.Dimension] = dimensions.map(d => d.name -> d)
    .collect({ case (Some(name), d) => name -> d })(breakOut)
}

trait HasAttributes {
  import Implicits._
  def attributes: IndexedSeq[nc2.Attribute]
  def attributeMap: Map[String, nc2.Attribute] = attributes.map(a => a.name -> a)(breakOut)
}

trait HasVariables {
  import Implicits._
  def variables: IndexedSeq[nc2.Variable]
  def variableMap: Map[String, nc2.Variable] = variables.map(a => a.name -> a)(breakOut)
}
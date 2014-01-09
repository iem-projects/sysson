package at.iem.sysson
package sonif

import de.sciss.synth.SynthGraph
import scala.swing.Frame
import de.sciss.guiflitz.AutoView

//object Patch {
//  val empty = withoutSource("<empty>", SynthGraph {})
//
//  case class Source(name: String, code: String)
//
//  def withoutSource(name: String, graph: SynthGraph): Patch =
//    Patch(source = Patch.Source(name = name, code = ""), graph = graph)
//}
//case class Patch(source: Patch.Source, graph: SynthGraph) {
//  def name = source.name
//}

/** Specification for a sonification data source.
  *
  * @param name         Logical name by which the source is referred to
  * @param dimensions   List of specifications of required dimensions
  * @param higherRank   Whether a matrix rank higher than `dimensions.size` is permitted
  */
case class DataSourceSpec(name: String, dimensions: Vec[DimensionSpec], higherRank: Boolean)

/** Specification of a data source dimension
  *
  * @param name     Logical name by which the dimension is referred to
  * @param minSize  Minimum domain size, or none
  * @param maxSize  Maximum domain size, or none
  */
case class DimensionSpec(name: String, minSize: Option[Int], maxSize: Option[Int])

sealed trait UserScalar {
  /** Logical name by which the value is referred to */
  def name: String
}

case class RangeMinimum(range: String, name: String) extends UserScalar
case class RangeMaximum(range: String, name: String) extends UserScalar
case class RangeMarker (range: String, name: String) extends UserScalar
case class DomainMarker(range: String, name: String) extends UserScalar

case class UserConstant(name: String) extends UserScalar // XXX TODO: continue here with numeric spec

case class Declaration(sources: Vec[DataSourceSpec] = Vec.empty, user: Vec[UserScalar] = Vec.empty)

object DeclarationEditor {
  def apply(): Unit = {
    val gui = AutoView(Declaration())

    new Frame {
      contents = gui.component
      pack().centerOnScreen()
      open()
    }
  }
}
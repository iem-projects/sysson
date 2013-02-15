package at.iem.sysson

import ucar.{nc2, ma2}

import Implicits._

object VariableSection {
  final class In(section: VariableSection, dim: Int) {
    object select {
      def apply(r: OpenRange) = "hallo " + r
//      def apply(at: OpenRange.at.type) = (i: Int) => at(i)
    }
  }
}
final class VariableSection(v: nc2.Variable, s: ma2.Section) {
  implicit def variable = v
  implicit def section  = s

  def read(): ma2.Array = v.read(s)

  def in(dim: String): VariableSection.In = {
    val idx = v.findDimensionIndex(dim)
    if (idx < 0) throw new IllegalArgumentException(s"Variable '${v.name}' has no dimension '$dim'")
    new VariableSection.In(this, idx)
  }

  override def toString = s"${v.name} @ $s"
}
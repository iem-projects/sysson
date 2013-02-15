package at.iem.sysson

import ucar.{nc2, ma2}
import collection.immutable.{IndexedSeq => IIdxSeq}

import Implicits._
import collection.JavaConversions

object VariableSection {
  final class In(vs: VariableSection, dim: Int) {
    object select {
      def apply(i: Int): VariableSection = apply(OpenRange.at(i))
      def apply(r: OpenRange): VariableSection = {
        vs.copy(section = vs.section.updated(dim, r))
      }
    }
  }
}
final case class VariableSection(variable: nc2.Variable, section: IIdxSeq[OpenRange]) extends impl.VariableLike {
  def read(): ma2.Array = variable.read(toSection)

  private lazy val toSection: ma2.Section = {
    val sz      = section.size
    val origin  = new Array[Int](sz)
    val shape   = new Array[Int](sz)
    val stride  = new Array[Int](sz)
    var i = 0; while (i < sz) {
      val range = section(i)
      origin(i) = math.max(0, range.startOption.getOrElse(0))
      val len   = variable.getShape(i)
      shape(i)  = math.max(0, math.min(len, range.stopOption.getOrElse(len)) - origin(i))
      stride(i) = range.step
    i += 1}
    new ma2.Section(origin, shape)
  }

  def ranges: IIdxSeq[ma2.Range] = {
    import JavaConversions._
    toSection.getRanges.toIndexedSeq
  }

  def size: Long = {
    val sh = shape
    var res = 1L
    var i = 0; while (i < sh.size) {
      res *= sh(i)
    i += 1 }
    res
  }

  def rank          = toSection.getRank
  def shape         = toSection.getShape.toIndexedSeq

  def name          = variable.name
  def dataType      = variable.dataType
  def dimensions    = variable.dimensions
  def selectAll     = variable.selectAll

  def in(dim: String): VariableSection.In = {
    val idx = variable.findDimensionIndex(dim)
    if (idx < 0) throw new IllegalArgumentException(s"Variable '${variable.name}' has no dimension '$dim'")
    new VariableSection.In(this, idx)
  }

  override def toString = {
    val relevant  = section.zipWithIndex.filterNot(_._1.isAll)
    if (relevant.isEmpty) variable.name else {
      val relT      = relevant.map { case (r, idx) => s"${variable.getDimension(idx).name.getOrElse(idx)}: $r" }
      s"${variable.name} @ ${relT.mkString("[", ", ", "]")}"
    }
  }
}
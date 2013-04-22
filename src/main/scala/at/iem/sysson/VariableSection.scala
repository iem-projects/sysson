package at.iem.sysson

import ucar.{nc2, ma2}
import collection.immutable.{IndexedSeq => IIdxSeq}

import Implicits._
import collection.JavaConversions
import at.iem.sysson.gui.Plot

object VariableSection {
  /** A transitory class specifying a variable section along with a dimension
    * on which a subsequent selection is to be made
    *
    * @param  vs  the variable section
    * @param  dim the dimension given as index into `vs.dimensions`
    */
  final class In(vs: VariableSection, dim: Int) {
    /** The select operation is given as an object with `apply` method */
    object select {
      /** Slices the variable section in the selected dimension at a given index */
      def apply(i: Int): VariableSection = apply(OpenRange.at(i))
      /** Constrains the variable section in the selected dimension to a given sub range */
      def apply(r: OpenRange): VariableSection = {
        vs.copy(section = vs.section.updated(dim, r))
      }
    }
  }
}

/** A variable section is a non destructive selection within the dimension of a variable,
  * possibly applying scaling to its values.
  *
  * @param variable the original NetCDF variable
  * @param section  the range selection within the dimension
  */
final case class VariableSection(variable: nc2.Variable, section: IIdxSeq[OpenRange], scale: Scale = Scale.Identity)
  extends impl.VariableLike {

  def readSafe(): ma2.Array = variable.synchronized(variable.read(toSection))

  def readScaled1D(): IIdxSeq[Float] = read().scaled1D(scale)

  def applyScale(scale: Scale): VariableSection = copy(scale = scale)

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

  /** Reports the total number of elements within the selected sub matrix */
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

  /** Undoes all dimensional selections and reverts to the full matrix of the variable */
  def selectAll     = variable.selectAll

  def file: nc2.NetcdfFile = variable.file

  /** Selects a dimension in which a subsequent selection should be made.
    * A typical call is `section in "dim-name" select some-range`
    *
    * @param dim  the dimension to operate on, specified as its name
    * @return an object holding the section along with the dimension selection,
    *         ready to apply a particular range selection
    */
  def in(dim: String): VariableSection.In = {
    new VariableSection.In(this, dimIdxByName(dim))
  }

  // ---- plotting ----

  def plot(): Plot = Plot(this)

  private def dimIdxByName(n: String): Int = {
    val idx = variable.findDimensionIndex(n)
    if (idx < 0) throw new IllegalArgumentException(s"Variable '${variable.name}' has no dimension '$n'")
    idx
  }

  // ---- conversion to sonifcation source ----
  def asColumn      = ColumnSource(this)
  def asRow         = RowSource(this)

  def asMatrix(row: String, column: String) =
    MatrixSource(this, rowDim = dimIdxByName(row), columnDim = dimIdxByName(column))

  override def toString = {
    val relevant = section.zipWithIndex.filterNot(_._1.isAll)
    val selected = if (relevant.isEmpty) variable.name else {
      val relT      = relevant.map { case (r, idx) => s"${variable.getDimension(idx).nameOption.getOrElse(idx)}: $r" }
      s"${variable.name} @ ${relT.mkString("[", ", ", "]")}"
    }
    if (scale == Scale.Identity) selected else s"$selected ; scale = $scale"
  }
}
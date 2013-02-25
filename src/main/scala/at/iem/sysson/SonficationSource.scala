package at.iem.sysson

sealed trait SonificationSource
import Implicits._

case class ColumnSource(section: VariableSection) extends SonificationSource {
  require(section.reducedRank == 1, s"Reduced rank must be 1 for a column source: $section")

  lazy val size = section.reducedShape.head

  override def toString = s"$section.asColumn"
}

case class RowSource(section: VariableSection) extends SonificationSource {
  require(section.reducedRank == 1, s"Reduced rank must be 1 for a row source: $section")

  lazy val size = section.reducedShape.head

  override def toString = s"$section.asRow"
}

case class MatrixSource(section: VariableSection, rowDim: Int, columnDim: Int) extends SonificationSource {
  require(section.reducedRank == 2, s"Reduced rank must be 2 for a matrix source: $section")
  require(section.variable.getDimension(rowDim).getLength > 1 && section.variable.getDimension(columnDim).getLength > 1,
    s"Row ($rowDim) or column ($columnDim) dimension will be reduced")

  override def toString = {
    val dims    = section.reducedDimensions
    val rowName = dims(rowDim   ).name.getOrElse(rowDim   )
    val colName = dims(columnDim).name.getOrElse(columnDim)
    s"""$section.asMatrix(row: "$rowName", col: "$colName")"""
  }
}
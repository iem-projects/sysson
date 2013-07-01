package at.iem.sysson

sealed trait SonificationSource {
  def numColumns:   Int
  def numRows:      Int
  def section:      VariableSection
  def isTransposed: Boolean
}

case class ColumnSource(section: VariableSection) extends SonificationSource {
  require(section.reducedRank == 1, s"Reduced rank must be 1 for a column source: $section")

  lazy val size     = section.reducedShape.head
  def numRows       = size
  def numColumns    = 1
  def isTransposed  = false

  override def toString = s"$section.asColumn"
}

case class RowSource(section: VariableSection) extends SonificationSource {
  require(section.reducedRank == 1, s"Reduced rank must be 1 for a row source: $section")

  lazy val size     = section.reducedShape.head
  def numRows       = 1
  def numColumns    = size
  def isTransposed  = false

  override def toString = s"$section.asRow"
}

case class MatrixSource(section: VariableSection, rowDim: Int, columnDim: Int) extends SonificationSource {
  import Implicits._

  require(section.reducedRank == 2, s"Reduced rank must be 2 for a matrix source: $section")
  require(section.dimensions(rowDim).size > 1 && section.dimensions(columnDim).size > 1,
    s"Row ($rowDim) or column ($columnDim) dimension will be reduced")

  def numRows       = section.dimensions(rowDim).size
  def numColumns    = section.dimensions(columnDim).size
  def isTransposed  = rowDim < columnDim  // XXX TODO: test if this is correct

  override def toString = {
    val dims    = section.reducedDimensions
    val rowName = dims(rowDim   ).nameOption.getOrElse(rowDim   )
    val colName = dims(columnDim).nameOption.getOrElse(columnDim)
    s"""$section.asMatrix(row: "$rowName", col: "$colName")"""
  }
}
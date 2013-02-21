package at.iem.sysson

sealed trait SonificationSource

case class ColumnSource(section: VariableSection) extends SonificationSource {
  require(section.reducedRank == 1, s"Reduced rank must be 1 for a column source: $section")

  lazy val size = section.reducedShape.head
}

case class RowSource(section: VariableSection) extends SonificationSource {
  require(section.reducedRank == 1, s"Reduced rank must be 1 for a row source: $section")

  lazy val size = section.reducedShape.head
}

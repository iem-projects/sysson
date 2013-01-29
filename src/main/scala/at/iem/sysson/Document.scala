package at.iem.sysson

import ucar.nc2

object Document {
  type Listener = Model.Listener[Update]

  sealed trait Update
  final case class Closed(doc: Document) extends Update
}

/**
 * A document represents one open data file
 */
trait Document extends Model[Document.Update] {
  /**
   * Path to the document's underlying file (NetCDF).
   */
  def path: String

  def data: nc2.NetcdfFile
  def variableMap: Map[String, nc2.Variable]

  def close(): Unit
}
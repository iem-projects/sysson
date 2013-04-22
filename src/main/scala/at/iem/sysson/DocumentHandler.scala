package at.iem.sysson

import impl.{DocumentHandlerImpl => Impl}
import de.sciss.model.Model

object DocumentHandler {
  lazy val instance: DocumentHandler = Impl()

  sealed trait Update
  final case class Opened(doc: Document) extends Update
  final case class Closed(doc: Document) extends Update
}
trait DocumentHandler extends Model[DocumentHandler.Update] {
  def openRead(path: String): Document
  def allDocuments: Iterator[Document]
  def getDocument(path: String): Option[Document]
}
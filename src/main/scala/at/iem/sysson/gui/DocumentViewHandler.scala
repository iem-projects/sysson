package at.iem.sysson
package gui

import impl.{DocumentViewHandlerImpl => Impl}
import de.sciss.model.Model

object DocumentViewHandler {
  lazy val instance: DocumentViewHandler = Impl.instance // Impl()

  sealed trait Update
  case class Activated(doc: Document) extends Update
}
trait DocumentViewHandler extends Model[DocumentViewHandler.Update] {
  def getView(doc: Document): Option[DocumentView]
  var activeDocument: Option[Document]
}
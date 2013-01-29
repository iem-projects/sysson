package at.iem.sysson
package gui

import impl.{DocumentViewHandlerImpl => Impl}

object DocumentViewHandler {
  lazy val instance: DocumentViewHandler = Impl.instance // Impl()
}
trait DocumentViewHandler {
  def getView(doc: Document): Option[DocumentView]
}
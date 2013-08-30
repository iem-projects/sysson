package at.iem.sysson
package gui
package impl

import de.sciss.model.impl.ModelImpl

private[gui] object DocumentViewHandlerImpl {
  def instance: DocumentViewHandler = impl

  private lazy val impl = new Impl

  def mkView(doc: Document): DocumentView = impl.mkView(doc)

  private final class Impl extends DocumentViewHandler with ModelImpl[DocumentViewHandler.Update] {
    override def toString = "DocumentViewHandler"

    private var map       = Map.empty[Document, DocumentView]
    private var _active  = Option.empty[Document]

    def activeDocument = _active
    def activeDocument_=(value: Option[Document]): Unit =
      if (_active != value) {
        _active = value
        value.foreach { doc =>
          dispatch(DocumentViewHandler.Activated(doc))
        }
      }

    def getView(doc: Document): Option[DocumentView] = {
      GUI.requireEDT()
      map.get(doc)
    }

    def mkView(doc: Document): DocumentView = {
      getView(doc).getOrElse {
        val view = DocumentViewImpl(doc)
        map += doc -> view
        doc.addListener {
          case Document.Closed(_) => GUI.defer {
            view.dispose()
            map -= doc
          }
        }
        view
      }
    }
  }
}
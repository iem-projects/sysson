package at.iem.sysson
package gui
package impl

private[gui] object DocumentViewHandlerImpl {
  def instance: DocumentViewHandler = impl

  private lazy val impl = new Impl

  def mkView(doc: Document): DocumentView = impl.mkView(doc)

  private final class Impl extends DocumentViewHandler {
    override def toString = "DocumentViewHandler"

    private var map = Map.empty[Document, DocumentView]

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
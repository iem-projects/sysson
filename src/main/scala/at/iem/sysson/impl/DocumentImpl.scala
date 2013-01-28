package at.iem.sysson
package impl

object DocumentImpl {
  def openRead(path: String): Document = new Impl(path)

  private final class Impl(val path: String) extends Document with ModelImpl[Document.Update] {
    def close() {
      dispatch(Document.Closed(this))
    }
  }
}
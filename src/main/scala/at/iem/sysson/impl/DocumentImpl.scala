package at.iem.sysson
package impl

import ucar.nc2

object DocumentImpl {
  def openRead(path: String): Document = {
    val f = nc2.NetcdfFile.open(path)
    new Impl(path, f)
  }

  private final class Impl(val path: String, val data: nc2.NetcdfFile)
    extends Document with ModelImpl[Document.Update] {

    override def toString = "Document(" + data.getTitle + ")"

    def close() {
      data.close()
      dispatch(Document.Closed(this))
    }
  }
}
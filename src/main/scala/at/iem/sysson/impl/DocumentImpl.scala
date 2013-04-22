package at.iem.sysson
package impl

import ucar.nc2
import de.sciss.model.impl.ModelImpl
import java.io.File

object DocumentImpl {
  import Implicits._

  def openRead(path: String): Document = {
    val f   = nc2.NetcdfFile.open(path)
    val vm  = f.variableMap // build that once
    new Impl(path, f, vm)
  }

  private final class Impl(val path: String, val data: nc2.NetcdfFile, val variableMap: Map[String, nc2.Variable])
    extends Document with ModelImpl[Document.Update] {

    override def toString = "Document(" + data.getTitle + ")"

    def file = new File(path)

    def close() {
      data.close()
      dispatch(Document.Closed(this))
    }
  }
}
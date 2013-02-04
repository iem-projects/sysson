package at.iem.sysson
package sound
package designer

import impl.{DesignerViewImpl => Impl}

object DesignerView {
  def apply(): DesignerView = Impl()
}
trait DesignerView
package at.iem.sysson

import at.iem.sysson.gui.impl.{SonificationObjView, DataSourceObjView}

package object gui {
  def registerViews(): Unit = {
    DataSourceObjView
    SonificationObjView
  }
}

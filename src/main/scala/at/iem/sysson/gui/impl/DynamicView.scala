package at.iem.sysson.gui
package impl

import swing.Component
import javax.swing.event.{AncestorEvent, AncestorListener}

trait DynamicView {
  protected def component: Component
  protected def componentOpened(): Unit
  protected def componentClosed(): Unit

  component.peer.addAncestorListener( new AncestorListener {
    def ancestorAdded(e: AncestorEvent) {
      componentOpened()
    }

    def ancestorRemoved(e: AncestorEvent) {
      componentClosed()
    }

    def ancestorMoved(e: AncestorEvent) {}
  })
}
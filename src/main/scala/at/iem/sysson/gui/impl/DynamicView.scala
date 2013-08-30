package at.iem.sysson.gui
package impl

import swing.Component
import javax.swing.event.{AncestorEvent, AncestorListener}

trait DynamicView {
  protected def component: Component
  protected def componentOpened(): Unit
  protected def componentClosed(): Unit

  component.peer.addAncestorListener(new AncestorListener {
    def ancestorAdded  (e: AncestorEvent): Unit = componentOpened()
    def ancestorRemoved(e: AncestorEvent): Unit = componentClosed()
    def ancestorMoved  (e: AncestorEvent)       = ()
  })
}
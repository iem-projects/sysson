package at.iem.sysson
package impl

import util.control.NonFatal

trait ModelImpl[U] extends Model[U] {
  private type Listener = Model.Listener[U]
  private val sync      = new AnyRef
  private var listeners = Vector.empty[Listener]

  final protected def dispatch(update: U) {
    sync.synchronized {
      listeners.foreach { pf =>
        if (pf.isDefinedAt(update)) try {
          pf(update)
        } catch {
          case NonFatal(e) => e.printStackTrace()
        }
      }
    }
  }

  final def addListener(pf: Listener) = sync.synchronized {
    listeners :+= pf
    pf
  }

  final def removeListener(pf: Listener) { sync.synchronized {
    val idx = listeners.indexOf(pf)
    if (idx >=0 ) listeners = listeners.patch(idx, Nil, 1)
  }}
}
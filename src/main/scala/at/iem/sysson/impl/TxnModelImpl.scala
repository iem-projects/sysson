/*
 *  TxnModelImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package impl

import de.sciss.lucre.stm.{Disposable, TxnLike}
import scala.concurrent.stm.Ref
import scala.util.control.NonFatal

trait TxnModelImpl[Tx <: TxnLike, U] {
  private class Observer(val fun: Tx => U => Unit) extends Disposable[Tx] {
    obs =>
    override def dispose()(implicit tx: Tx): Unit =
      listeners.transform { v =>
        val idx = v.indexOf(obs)
        if (idx < 0) v else v.patch(idx, Nil, 1)
      } (tx.peer)
  }

  private val listeners = Ref(Vec.empty[Observer])

  final def react(fun: Tx => U => Unit)(implicit tx: Tx): Disposable[Tx] = {
    val res = new Observer(fun)
    listeners.transform(_ :+ res)(tx.peer)
    res
  }

  final protected def releaseListeners()(implicit tx: Tx): Unit = listeners.set(Vec.empty)(tx.peer)

  final protected def dispatch(update: U)(implicit tx: Tx): Unit =
    listeners.get(tx.peer).foreach { obs =>
      try {
        obs.fun(tx)(update)
      } catch {
        case NonFatal(e) =>
          e.printStackTrace()
      }
    }
}

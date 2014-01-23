/*
 *  AuralSonificationImpl.scala
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
package sound
package impl

import de.sciss.lucre.event.Sys
import at.iem.sysson.sound.AuralSonification.{Update, Playing, Stopped, Preparing}
import at.iem.sysson.impl.TxnModelImpl
import scala.concurrent.stm.Ref
import de.sciss.lucre.stm

object AuralSonificationImpl {
  def apply[S <: Sys[S]](aw: AuralWorkspace[S], sonification: Sonification[S])
                        (implicit tx: S#Tx): AuralSonification[S] = {
    // aw.workspace.cursor
    val sonifH = tx.newHandle(sonification)
    new Impl(sonifH)
  }

  // private sealed trait State
  // private case object Stopped extends State

  private final class Impl[S <: Sys[S]](sonifH: stm.Source[S#Tx, Sonification[S]])
    extends AuralSonification[S] with TxnModelImpl[S#Tx, Update] {

    private val _state = Ref(Stopped: Update)

    def state(implicit tx: S#Tx): Update = _state.get(tx.peer)

    def stop()(implicit tx: S#Tx): Unit = {
      val oldState = _state.swap(Stopped)(tx.peer)
      if (oldState != Stopped) dispatch(Stopped)
    }

    def play()(implicit tx: S#Tx): Unit = {
      stop()
      prepare()
    }

    private def prepare()(implicit tx: S#Tx): Unit = {
      val sonif = sonifH()
      val graph = sonif.patch.graph.value

    }
  }
}

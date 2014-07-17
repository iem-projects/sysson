/*
 *  AuralSonificationImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package sound
package impl

import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.AuralObj.State
import de.sciss.synth.proc.impl.ObservableImpl
import de.sciss.synth.proc.{TimeRef, AuralContext, AuralObj}
import de.sciss.lucre.{event => evt, stm}

object AuralSonificationImpl extends AuralObj.Factory {
  AuralObj.addFactory(this)

  def typeID = Sonification.typeID
  type E[S <: evt.Sys[S]] = Sonification.Elem[S]

  def apply[S <: Sys[S]](obj: Sonification.Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = {
    println(s"AuralSonification($obj)")
    val objH      = tx.newHandle(obj)
    val procView  = AuralObj(obj.elem.peer.proc)
    val res       = new Impl(objH, procView)
    res.init()
    res
  }

  private final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Sonification.Obj[S]], procView: AuralObj[S])
    extends AuralObj[S] with ObservableImpl[S, AuralObj.State] {

    def typeID = Sonification.typeID

    private var procViewL: Disposable[S#Tx] = _

    def init()(implicit tx: S#Tx): Unit = {
      procViewL = procView.react { implicit tx => upd =>
        fire(upd) // just forward
      }
    }

    def play(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      println("AuralSonification: play")
      procView.play(timeRef)
    }

    def stop()(implicit tx: S#Tx): Unit = {
      println("AuralSonification: stop")
      procView.stop()
    }

    def state(implicit tx: S#Tx): State = {
      procView.state
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      procViewL.dispose()
      procView .dispose()
    }
  }
}

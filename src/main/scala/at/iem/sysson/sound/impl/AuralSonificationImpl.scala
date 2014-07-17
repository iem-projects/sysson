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

import at.iem.sysson.graph.UserValue
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.AuralObj.State
import de.sciss.synth.proc.UGenGraphBuilder.{Incomplete, Input}
import de.sciss.synth.proc.impl.{AuralProcImpl, AuralProcDataImpl, ObservableImpl}
import de.sciss.synth.proc.{UGenGraphBuilder, TimeRef, AuralContext, AuralObj}
import de.sciss.lucre.{event => evt, stm}

object AuralSonificationImpl extends AuralObj.Factory {
  AuralObj.addFactory(this)

  def typeID = Sonification.typeID
  type E[S <: evt.Sys[S]] = Sonification.Elem[S]

  def apply[S <: Sys[S]](obj: Sonification.Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = {
    println(s"AuralSonification($obj)")
    val objH      = tx.newHandle(obj)
    val proc      = obj.elem.peer.proc
    val procData  = context.acquire[AuralObj.ProcData[S]](proc)(new ProcDataImpl[S].init(proc))
    val procView  = new ProcImpl[S].init(procData)
    val res       = new Impl(objH, procView)
    res.init()
    res
  }

  private final class ProcDataImpl[S <: Sys[S]](implicit context: AuralContext[S])
    extends AuralProcDataImpl.Impl[S]() {

    override def requestInput[Res](req: Input { type Value = Res }, st: Incomplete[S])
                                  (implicit tx: S#Tx): Res = req match {
      case uv: UserValue => UGenGraphBuilder.Unit
      case _ => super.requestInput(req, st)
    }
  }

  private final class ProcImpl[S <: Sys[S]](implicit context: AuralContext[S])
    extends AuralProcImpl.Impl[S]() {

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

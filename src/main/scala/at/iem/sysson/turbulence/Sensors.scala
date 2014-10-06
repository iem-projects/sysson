/*
 *  Sensors.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson.turbulence

import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.synth.{Bus, ControlBus, Server, Txn}
import de.sciss.mellite.Mellite
import de.sciss.synth
import de.sciss.synth.proc.{SensorSystem, AuralSystem}
import Mellite.{auralSystem, sensorSystem}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.Ref

object Sensors {
  final val NumChannels = 20  // XXX TODO - should reflect prefs

  private[turbulence] def init()(implicit tx: Txn): Unit = {
    sensorSystem.addClient(Impl)
    auralSystem .addClient(Impl)
  }

  private val busRef    = Ref(Option.empty[synth.ControlBus])
  private val serverRef = Ref(Option.empty[Server])

  def bus: synth.ControlBus =
    busRef.single.get.getOrElse(sys.error("Aural system not running - no sensor bus"))

  private object Impl extends SensorSystem.Client with AuralSystem.Client with ControlBus.User {

    def sensorsStarted(c: SensorSystem.Server)(implicit tx: TxnLike): Unit = ()

    def sensorsStopped()(implicit tx: TxnLike): Unit = {
      implicit val itx = tx.peer
      for {
        cBus <- busRef()
      } {
        clearBus(cBus)
      }
    }

    def sensorsUpdate(values: Vec[Float])(implicit tx: TxnLike): Unit = {
      val valuesT = if (values.size <= NumChannels) values else values.take(NumChannels)
      implicit val itx = tx.peer
      for {
        cBus <- busRef()
        s    <- serverRef()
      } {
        s ! cBus.setnMsg(valuesT)
      }
    }

    def auralStarted(s: Server)(implicit tx: Txn): Unit = {
      implicit val itx = tx.peer
      serverRef() = Some(s)
      val bus = Bus.control(s, NumChannels)
      busRef() = None
      bus.addReader(this) // let's keep it simple
      bus.addWriter(this)
    }

    def auralStopped()(implicit tx: Txn): Unit = {
      implicit val itx = tx.peer
      busRef()    = None
      serverRef() = None
    }

    def busChanged(cBus: synth.ControlBus)(implicit tx: Txn): Unit = {
      busRef.set(Some(cBus))(tx.peer)
      clearBus(cBus)
    }

    private def clearBus(cBus: synth.ControlBus)(implicit tx: TxnLike): Unit =
      serverRef.get(tx.peer).foreach(_ ! cBus.fillMsg(0f))
  }
}
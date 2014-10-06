/*
 *  SensorIn.scala
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

import de.sciss.synth
import de.sciss.synth.{UGenInLike, ControlRated, GE}

object SensorIn {
  def kr: SensorIn = kr()
  def kr(offset: Int = 0, numChannels: Int = Sensors.NumChannels): SensorIn =
    apply(offset = offset, numChannels = numChannels)
}
final case class SensorIn(offset: Int, numChannels: Int) extends GE.Lazy with ControlRated {
  protected def makeUGens: UGenInLike = {
    import synth._
    import ugen._
    val bus = Sensors.bus
    In.kr(bus.index + offset, numChannels)
  }
}
/*
 *  UserValue.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson.graph

import de.sciss.synth.proc.UGenGraphBuilder
import de.sciss.synth.ugen.ControlProxy
import de.sciss.synth.{control, audio, Rate, scalar, GE, UGenInLike}
import de.sciss.synth

object UserValue {
  case class GE(rate: Rate, peer: UserValue) extends synth.GE.Lazy /* with SonificationElement */ {
    override def productPrefix = s"UserValue$$GE"

    protected def makeUGens: UGenInLike = {
      val b       = UGenGraphBuilder.get
      b.requestInput(peer)
      val ctlName = controlName(peer.name)
      ControlProxy(rate, Vector(peer.default.toFloat), Some(ctlName))
    }
  }

  /** Same as `UserValue(name, default).ir` */
  def ir(name: String, default: Double): synth.GE = UserValue(name, default).ir
  /** Same as `UserValue(name, default).kr` */
  def kr(name: String, default: Double): synth.GE = UserValue(name, default).kr
  /** Same as `UserValue(name, default).ar` */
  def ar(name: String, default: Double): synth.GE = UserValue(name, default).ar

  case class Key(name: String) extends UGenGraphBuilder.Key {
    override def productPrefix = "UserValue.Key"
  }

  private[sysson] def controlName(key: String): String = s"$$user_$key"
}
final case class UserValue(name: String, default: Double)
  extends UserInteraction with UGenGraphBuilder.Input {

  def ir   : GE = UserValue.GE(scalar , this)
  def kr   : GE = UserValue.GE(control, this)
  def ar   : GE = UserValue.GE(audio  , this)

  type Key    = UserValue.Key
  type Value  = UGenGraphBuilder.Unit

  def key     = UserValue.Key(name)
}
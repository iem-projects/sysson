/*
 *  AudioSystem.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package sound

import impl.{AudioSystemImpl => Impl}
import de.sciss.synth
import synth.Server
import de.sciss.osc.TCP
import de.sciss.model.Model

object AudioSystem {
  def instance: AudioSystem = Impl.instance

  def start(config: synth.Server.Config = defaultConfig): AudioSystem = instance.start(config)

  sealed trait Update
  final case class Booting(connection: synth.ServerConnection) extends Update
  final case class Started(server    : synth.Server          ) extends Update
  case object      Stopped                                     extends Update

  type Listener = Model.Listener[Update]

  lazy val defaultConfig = {
    val cfg         = synth.Server.Config()
    cfg.transport   = TCP   // allows larger SynthDefs somehow
    cfg.wireBuffers = 4096  // make it possible to have massively multi-channel buffers
    cfg.pickPort()
    cfg.build
  }
}
trait AudioSystem extends Model[AudioSystem.Update] {
  def server: Option[synth.ServerLike]
  def start(config: synth.Server.Config = AudioSystem.defaultConfig): this.type
  def stop(): this.type

  def isBooting: Boolean
  def isRunning: Boolean

  def whenBooted(fun: Server => Unit): this.type
}
/*
 *  AudioSystem.scala
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

import impl.{AudioSystemImpl => Impl}
import de.sciss.synth
import de.sciss.osc.TCP
import de.sciss.model.Model
import de.sciss.lucre.synth.Server
import de.sciss.synth.proc.AuralSystem
import de.sciss.file._

object AudioSystem {
  def instance: AudioSystem = Impl.instance

  def start(config: synth.Server.Config = defaultConfig): AudioSystem = instance.start(config)

  sealed trait Update
  // final case class Booting(connection: synth.ServerConnection) extends Update
  final case class Started(server    : Server          ) extends Update
  case object      Stopped                               extends Update

  type Listener = Model.Listener[Update]

  def defaultConfig: synth.Server.Config = {
    val config          = synth.Server.Config()
    val programPath     = Prefs.superCollider.getOrElse(Prefs.defaultSuperCollider)
    if (programPath != Prefs.defaultSuperCollider) config.program = programPath.path
    val audioDevice     = Prefs.audioDevice.getOrElse(Prefs.defaultAudioDevice)
    if (audioDevice != Prefs.defaultAudioDevice) config.deviceName = Some(audioDevice)
    config.outputBusChannels = Prefs.audioNumOutputs.getOrElse(Prefs.defaultAudioNumOutputs)
    config.wireBuffers  = 4096  // make it possible to have massively multi-channel buffers
    config.transport    = TCP   // allows larger SynthDefs somehow
    config.pickPort()
    config.build
  }
}
trait AudioSystem extends Model[AudioSystem.Update] {
  private[sysson] def aural: AuralSystem    // bridge to SoundProcesses

  def server: Option[Server]
  def start(config: synth.Server.Config = AudioSystem.defaultConfig): this.type
  def stop(): this.type

  // def isBooting: Boolean
  def isRunning: Boolean

  def whenBooted(fun: Server => Unit): this.type
}
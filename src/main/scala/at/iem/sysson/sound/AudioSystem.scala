package at.iem.sysson
package sound

import impl.{AudioSystemImpl => Impl}
import de.sciss.synth
import synth.Server

object AudioSystem {
  def instance: AudioSystem = Impl.instance

  sealed trait Update
  final case class Booting(connection: synth.ServerConnection) extends Update
  final case class Started(server: synth.Server) extends Update
  case object Stopped extends Update

  type Listener = Model.Listener[Update]
}
trait AudioSystem extends Model[AudioSystem.Update] {
  def server: Option[synth.ServerLike]
  def start(config: synth.Server.Config = synth.Server.Config().build): this.type
  def stop(): this.type

  def isBooting: Boolean
  def isRunning: Boolean

  def whenBooted(fun: Server => Unit): this.type
}
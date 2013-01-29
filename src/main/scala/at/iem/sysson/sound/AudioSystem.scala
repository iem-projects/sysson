package at.iem.sysson
package sound

import impl.{AudioSystemImpl => Impl}
import de.sciss.synth

object AudioSystem {
  def instance: AudioSystem = Impl.instance

  sealed trait Update
  final case class Started(server: synth.Server) extends Update
  case object Stopped extends Update
}
trait AudioSystem extends Model[AudioSystem.Update] {
  def start(): Unit
  def stop(): Unit
  def isRunning: Boolean
}
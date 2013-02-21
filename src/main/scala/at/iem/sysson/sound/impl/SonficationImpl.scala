package at.iem.sysson
package sound
package impl

import de.sciss.synth._
import concurrent.duration.Duration

object SonficationImpl {
  def playOver(sonif: Sonification, duration: Duration): Synth = {
    val szs = sonif.mapping.values.collect {
      case r @ RowSource(_) => r.size
    }
    val sz    = szs.headOption.getOrElse(sys.error("playOver requires at least one row vector"))
    val rate  = sz.toDouble / duration.toMillis
    play(sonif, rate = rate)
  }

  def play(sonif: Sonification, rate: Double): Synth = {
    val s = AudioSystem.instance.server match {
      case Some(_s: Server) => _s
      case _ => sys.error("Audio system not running")
    }
    ???
  }
}
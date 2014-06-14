package at.iem.sysson
package graph

import de.sciss.synth._
import de.sciss.synth.ugen._

object output {
  def :=(in: GE): Lazy = Signal(in)

  final case class Signal(in: GE) extends Lazy.Expander[Unit] with HasSideEffect with IsIndividual {
    override def productPrefix  = "output$Signal"
    override def toString       = s"output := $in"

    protected def makeUGens: Unit = {
      val mute  = "$son_out".kr(0)
      val amp   = /* Lag.ar( */ 1 - mute /* , 0.05) */   // note: Lag.ar needs ar argument
      val sig   = in * amp
      Out.ar(0, sig)
    }
  }
}

/*
 *  output.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2016 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package graph

import de.sciss.synth._
import de.sciss.synth.proc.ObjKeys
import de.sciss.synth.proc.graph.{Attribute, FadeIn}
import de.sciss.synth.ugen._

object output {
  def :=(in: GE): Lazy = Signal(in)

  final case class Signal(in: GE) extends Lazy.Expander[Unit] with HasSideEffect with IsIndividual {
    override def productPrefix  = "output$Signal"
    override def toString       = s"output := $in"

    // XXX TODO - this refers to the Proc but not Sonification attrMap.
    // (which is correct; but the GUI view will currently update the
    //  sonification's dictionary)
    protected def makeUGens: Unit = {
      val mute  = Attribute.kr(ObjKeys.attrMute)
      val gain  = Attribute.kr(ObjKeys.attrGain, 1.0)
      val amp   = gain * (1 - mute)
      // XXX TODO - Fade out is broken for indefinite Duration
      // val env   = FadeInOut(ObjKeys.attrFadeIn, ObjKeys.attrFadeOut).ar
      val env   = FadeIn.ar
      val inS   = Seq(in\0, in\1): GE
      val sig   = inS * env * amp
      Out.ar(0, sig)
    }
  }
}

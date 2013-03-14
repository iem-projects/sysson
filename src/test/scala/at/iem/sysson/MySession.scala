package at.iem.sysson

import Implicits._
import de.sciss.synth
import synth._
import ugen._
import synth.Ops._
import concurrent.duration._

object MySession extends SessionLike {
  def run() {
    val x = play {
      FreeSelf.kr(MouseButton.kr)
      WhiteNoise.ar(SinOsc.ar(MouseX.kr.linexp(0, 1, 1, 1000)) * 0.5)
    }
    println(x)

    x.onEnd { quit() }
  }
}
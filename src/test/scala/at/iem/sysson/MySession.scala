package at.iem.sysson

import de.sciss.synth
import synth._
import ugen._

object MySession extends SessionLike {
  def run(): Unit = {
    import Ops._
    val x = play {
      FreeSelf.kr(MouseButton.kr)
      WhiteNoise.ar(SinOsc.ar(MouseX.kr.linExp(0, 1, 1, 1000)) * 0.5)
    }
    println(x)

    x.onEnd { quit() }
  }
}
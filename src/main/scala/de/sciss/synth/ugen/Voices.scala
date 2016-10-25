package de.sciss.synth.ugen

import de.sciss.synth.{ControlRated, GE, UGenInLike}

/**
  * Example:
  *
  * {{{
  *   val vcs     = Voices.T2(4)
  *   val an      = vcs.analyze(freqIn, ampIn, cmp = (vcs.in1 absdif freqIn) < maxDf)
  *   val env     = Env.asr(attack = egAtk, release = egRls)
  *   val eg      = EnvGen.ar(env, gate = an.active)
  *   val active  = A2K.kr(eg) sig_!= 0
  *   an.close(active)
  *   val lag     = an.activated * lagTime
  *   val freq    = Lag.ar(an.out1, lag)
  *   val osc     = SinOsc.ar(freq) * an.out2 * eg
  *   Out.ar(0, osc)
  *
  * }}}
  */
object Voices {
  case class T1(num: Int, global0: GE = List.empty[GE]) extends Voices {
    private[synth] def expand: UGenInLike = {
      LocalIn.kr(Seq.fill[Constant](num * numFeatures)(0) ++ global0.expand.outputs)
    }

    def numFeatures = 1

    def global = ??? : GE

    def analyze(in0: GE)(cmp: GE): A1 = ???
  }

  case class A1(in: T1, cmp: GE) extends Analysis {
    private[synth] def expand: UGenInLike = {
//      var activated   = Vector.fill(numVoices)(0: GE): GE
//      val noFounds = (0 until numTraj).map { tIdx =>
//        val idIn        = identIn \ tIdx
//        val fIn         = freqIn  \ tIdx
//        val aIn         = ampIn   \ tIdx
//        val isOn        = idIn > 0
//        val idMatch     = voiceId sig_== idIn
//        val bothOn      = voiceOnOff & isOn
//        val bestIn      = 0 +: (idMatch * (bothOn & !activated))
//        val best        = ArrayMax.kr(bestIn)
//        val bestIdx     = best.index - 1
//
//        val bestMask    = voiceNos sig_== bestIdx
//        activated      |= bestMask
//        val bestMaskN   = !bestMask
//        voiceId         = voiceId   * bestMaskN + idIn * bestMask
//        voiceFreq       = voiceFreq * bestMaskN + fIn  * bestMask
//        voiceAmp        = voiceAmp  * bestMaskN + aIn  * bestMask
//
//        bestIdx sig_== -1
//      }
//
//      for (tIdx <- 0 until numTraj) {
//        val idIn            = identIn \ tIdx
//        val fIn             = freqIn  \ tIdx
//        val aIn             = ampIn   \ tIdx
//        val isOn            = idIn > 0
//        val voiceAvail      = !(activated | voiceOnOff)
//        val notFound        = noFounds(tIdx)
//        val startTraj       = notFound & isOn
//        val free            = ArrayMax.kr(0 +: (startTraj & voiceAvail))
//        val freeIdx         = free.index - 1
//        val freeMask        = voiceNos sig_== freeIdx
//        activated          |= freeMask
//        val freeMaskN       = !freeMask
//        voiceId             = voiceId   * freeMaskN + idIn * freeMask
//        voiceFreq           = voiceFreq * freeMaskN + fIn  * freeMask
//        voiceAmp            = voiceAmp  * freeMaskN + aIn  * freeMask
//      }
      ???
    }

    def active    : GE = ???
    def activated : GE = ???
    def out       : GE = ???
  }

  trait Analysis extends GE with ControlRated {
    def in        : Voices
    def cmp       : GE

    def active    : GE
    def activated : GE
  }

  case class Out(a: Analysis, active: GE = 0, global: GE = List.empty[GE])
}
trait Voices extends GE with ControlRated {
  def num: Int
  def global: GE
  def numFeatures: Int
}
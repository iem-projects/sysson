/*
 *  Voices.scala
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

package de.sciss.synth
package ugen

trait VoicesExample {
  val freqIn  : GE = Seq(123, 345, 0)
  val ampIn   : GE = Seq(0.2, 0.3, 0.0)
  val maxDf   : GE = 50.0
  val lagTime : GE = 1.0

  val vcs     = Voices.T2(4)
  val an      = vcs.analyze(freqIn, ampIn)((vcs.in1 absdif freqIn) < maxDf)
  val env     = Env.asr()
  val eg      = EnvGen.ar(env, gate = an.active)
  val active  = A2K.kr(eg) sig_!= 0
  an.close(active)
  val lag     = an.activated * lagTime
  val freq    = Lag.ar(an.out1, lag)
  val osc     = SinOsc.ar(freq) * an.out2 * eg
  Out.ar(0, osc)
}

/** Building blocks for polyphony (voice) management.
  *
  * Example:
  *
  * {{{
  *   val vcs     = Voices.T2(4)
  *   val an      = vcs.analyze(freqIn, ampIn)((vcs.in1 absdif freqIn) < maxDf)
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
  case class T1(num: Int) extends Voices {
    def features = 1

    def in: GE = featureIn(0)

    def analyze(in: GE)(cmp: GE): A1 = A1(this, in = in, cmp = cmp)
  }

  case class T2(num: Int) extends Voices {
    def features = 2

    def in1: GE = featureIn(0)
    def in2: GE = featureIn(1)

    def analyze(in1: GE, in2: GE)(cmp: GE): A2 = A2(this, in1 = in1, in2 = in2, cmp = cmp)
  }

  case class A1(voices: T1, in: GE, cmp: GE) extends Analysis {
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

    def out: GE = featureOut(0)
  }

  case class A2(voices: T2, in1: GE, in2: GE, cmp: GE) extends Analysis {
    private[synth] def expand: UGenInLike = ???

    def out1      : GE = featureOut(0)
    def out2      : GE = featureOut(1)
  }

  trait Analysis extends GE with ControlRated {
    val voices    : Voices
    def cmp       : GE

    def active    : GE = featureOut(voices.features)
    def activated : GE = active & !voices.active
    def sustained : GE = active &  voices.active

    def close(active: GE = 0): Unit = Out(this, active = active)

    /*


     */

    final def featureOut(idx: Int): GE =
      ChannelRangeProxy(this, from = idx * voices.num, until = (idx + 1) * voices.num)
  }

  case class Out(analysis: Analysis, active: GE = 0) extends Lazy.Expander[Unit] {
    protected def makeUGens: Unit = {
      val newActive   = analysis.activated | active
      import analysis.voices.{features, num}
      val newFeatures = ChannelRangeProxy(analysis, from = 0, until = features * num)
      val combined    = Flatten(Seq(newFeatures, newActive))
      LocalOut.kr(combined)
    }
  }
}
trait Voices extends GE with ControlRated {
  /** Number of voices allocated. */
  def num: Int

  /** Number of features (graph element components) per voice. */
  def features: Int

  /*
      organization of the channels - features are joined together (this is an arbitrary decision):

      [vc0_feat0, vc1_feat0, ... vcN_feat0, vc0_feat1, vc1_feat1, ... vcN_feat1,
       ..., vc0_featM, vc1_featM, ..., vcN_featM]

      thus

         def get(vcIdx: Int, featIdx: Int) = ChannelProxy(this, featIdx * num + vcIdx)
         def get(featIdx: Int) = ChannelRangeProxy(this, from = featIdx * num, until = featIdx * num + num)

      Apart from the nominal number of features, `features`, there is an additional
      "feature" which is the on-off state of the voice. Thus we use
      `(num + 1) * features` channels for the local-in/out, and we can
      use `featureIn(features)` to obtain the on-off vector.

   */

  final def featureIn(idx: Int): GE =
    ChannelRangeProxy(this, from = idx * num, until = (idx + 1) * num)

  def active: GE = featureIn(features)

  private[synth] def expand: UGenInLike =
    LocalIn.kr(Seq.fill[Constant]((num + 1) * features)(0))
}
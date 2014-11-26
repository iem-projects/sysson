val DEBUG = false

val vPr = Var("pr")
val vTa = Var("tas")

val dPr = Dim(vPr, "time")
val dTa = Dim(vTa, "time")

val dimLat = Dim(vPr, "lat")

val speed = UserValue("speed", 1).kr

val tPr = dPr.play(speed)
val tTa = dTa.play(speed)

val pr  = vPr.play(tPr)
val ta  = vTa.play(tTa)

val lat = pr.axis(dimLat).values

if (DEBUG)
  (pr \ 0).poll(Impulse.kr(speed), "pr-anom")

val thrPr   = UserValue("thresh-pr", 1e-5).kr
val minPr   = -2.29e-4
val maxPr   = +4.39e-4

//val thrTa   =  0.15
val thrTa   = UserValue("thresh-tas", 0.15).kr
val minTa   = -17.6
val maxTa   = +15.7

val minLoPr = minPr
val maxLoPr = -thrPr
val minHiPr = thrPr
val maxHiPr = maxPr

val minLoTa = minTa
val maxLoTa = -thrTa
val minHiTa = thrTa
val maxHiTa = maxTa

val prHi      = pr > minHiPr
val prLo      = pr < maxLoPr

val taHi      = ta > minHiTa
val taLo      = ta < maxLoTa

val taGate    = taHi + taLo

val prLoNorm  = pr.clip(minLoPr, maxLoPr).linlin(minLoPr, maxLoPr, 1, 0)
val prHiNorm  = pr.clip(minHiPr, maxHiPr).linlin(minHiPr, maxHiPr, 0, 1)

val taLoNorm  = ta.clip(minLoTa, maxLoTa).linlin(minLoTa, maxLoTa, 1, 0)
val taHiNorm  = ta.clip(minHiTa, maxHiTa).linlin(minHiTa, maxHiTa, 0, 1)

val denLoMin  = UserValue("density-low-min", 1).kr
val denLoMax  = UserValue("density-low-max", 5).kr
val denHiMin  = UserValue("density-high-min", 10).kr
val denHiMax  = UserValue("density-high-max", 40).kr

val density   = prLoNorm.linlin(0, 1, denLoMax, denLoMin) * prLo +
                prHiNorm.linlin(0, 1, denHiMin, denHiMax) * prHi
                
val atk       = 0.002 * prLo +
                0.050 * prHi
val rls       = atk * 4
val sus       = prLoNorm.linlin(0, 1, 0.01, 0.00) * prLo +
                prHiNorm.linlin(0, 1, 0.02, 0.05) * prHi

// val dust = Impulse.ar(speed)

val pchLoMin  = UserValue("midi-low-min", 60).kr
val pchLoMax  = UserValue("midi-low-max", 68).kr
val pchHiMin  = UserValue("midi-high-min", 76).kr
val pchHiMax  = UserValue("midi-high-max", 84).kr

val pitch0  = taLoNorm.linlin(0, 1, pchLoMax, pchLoMin).midicps * taLo +
              taHiNorm.linlin(0, 1, pchHiMin, pchHiMax).midicps * taHi
val pitch1   = Gate.ar(pitch0, pitch0 > 0)
val pitch    = pitch1 // Lag.ar(pitch1)

// pitch.poll(1, "pitch")

// val dec  = Decay2.ar(dust, attack = atk, release = 0.2).min(1)

val numVoices = 8

def mkVoices(): GE = {
  val dustM = Dust.ar(density)
  Mix.tabulate(numVoices) { vc =>
    // distribute the input trigger `dust` among `numVoices` voices
    val tr   = PulseDivider.ar(dustM, div = numVoices, start = vc)
    val atk1 = Gate.ar(atk, tr)
    val sus1 = Gate.ar(sus, tr)
    val rls1 = Gate.ar(rls, tr)
    val pch1 = Gate.ar(pitch, tr)
    val env  = Env.linen(attack = atk1, sustain = sus1, release = rls1, curve = Curve.cubed)
    val eg   = EnvGen.ar(env, gate = tr)
    val amp  = Gate.ar(taGate, tr)
    val sig  = eg * SinOsc.ar(pch1) * amp
    sig
  }
}

val sig0 = mkVoices()
val pan  = lat.linlin(-90, 90, -1, 1)
val sig  = Pan2.ar(sig0, pan)

val amp  = UserValue("amp [dB]", -12).kr.dbamp

output := Mix(sig) * amp


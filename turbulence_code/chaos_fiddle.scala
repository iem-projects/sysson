play { LatoocarfianN.ar(MouseX.kr(20, SampleRate.ir)) * 0.2 }


play { LatoocarfianN.ar(MouseX.kr(20, SampleRate.ir)) * 0.2 }

play { GbmanN.ar(MouseX.kr(20, SampleRate.ir)) * 0.1 }

val b = Buffer.alloc(s, (0.2 * s.sampleRate).toInt.nextPowerOfTwo)

play { BufAllpassC.ar(b.id, WhiteNoise.ar(0.1), XLine.kr(0.0001, 0.01, 20), 0.2) }

play { 
  val sig = GbmanN.ar(MouseX.kr(20, SampleRate.ir)) * 0.1
  BufAllpassC.ar(b.id, sig, 0.2, 0.2)
}

play {
  val freq = MouseX.kr(400, SampleRate.ir * 0.25, 1) * WhiteNoise.ar.linexp(-1, 1, 0.5, 2.0)
  val sig = CuspL.ar(freq, 1.01, 1.91) * 0.3
  // val sig = GbmanN.ar(MouseX.kr(20, SampleRate.ir)) * 0.1
  // val sig = LatoocarfianN.ar(MouseX.kr(20, 1000 /* SampleRate.ir */, 1)) * 0.2 
  val v = sig // FreeVerb.ar(sig, mix = 0.5, room = 0.48, damp = 0.75)
  val out = HPZ1.ar(Resonz.ar(v, 600) + Resonz.ar(v, 3000)) * 4
  Pan2.ar(out)
}

play { HenonC.ar(MouseX.kr(20, SampleRate.ir)) * 0.2 }

play { LinCongL.ar(MouseX.kr(20, SampleRate.ir)) * 0.2 }

play { Logistic.ar(Line.kr(3.55, 4.0, 15), 1000) * 0.5 }  

play { LorenzL.ar(MouseX.kr(20, SampleRate.ir)) * 0.3 }

play {
  LorenzL.ar(
    SampleRate.ir,
    LFNoise0.kr(1).madd(2, 10),
    LFNoise0.kr(1).madd(20, 38),
    LFNoise0.kr(1).madd(1.5, 2)
  ) * 0.2
}

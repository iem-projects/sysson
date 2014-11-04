
    def mkTransition(fun: (GE, GE, GE) => GE): Unit = {
      val pred  = In.ar("pred".kr, 1)
      val succ  = In.ar("succ".kr, 1)
      val state = "state".kr(2)
      val target = 3 - state // 1 for fade-in, 0 for fade-out
      val start  = 1 - target
      val atk    = 20 // Attack
      val rls    = 20 // Release
      val in     = Select.kr(ToggleFF.kr(1), Seq(start, target))
      val fade   = Slew.kr(in, atk.reciprocal, rls.reciprocal)
      // if (DEBUG) fade.poll(1, "fade")
      val done   = fade sig_== target
      // graph.Action(done, "done")      
      val sig   = fun(pred, succ, fade)
      // graph.ScanOut(sig)
      Out.ar(0, Pan2.ar(sig))
    }
    
    val FFTSize = 512
    
    def mkBlend(pred: GE, succ: GE, z: GE, fade: GE, dt: GE): GE = {
      val dpa = fade.min(0.1) * 10
      val pa = 1 - dpa
      val sa = (fade - 0.9).max(0.0) * 10
      val dsa = 1 - sa
      val za = 1 - (pa max sa)
      // val pm = pred * pa
      // val sm = succ * sa
      val dp = if (dt == Constant(0)) pred else DelayN.ar(pred, dt, dt * dpa)
      val sp = if (dt == Constant(0)) succ else DelayN.ar(succ, dt, dt * dsa)
      val pm = dp * pa
      val sm = sp * sa
      val zm = z    * za
      //       pa.poll(1, "pa")
      //       sa.poll(1, "sa")
      //       za.poll(1, "za")
      pm + sm + zm
    }
    
    // transition 3: rising PV_MagBelow
    def t3(): Unit = mkTransition { (pred, succ, fade) =>
      val thresh  = fade.linexp(0, 1, 1.0e-3, 1.0e1)
      val bufPred = LocalBuf(FFTSize)
      val bufSucc = LocalBuf(FFTSize)
      val fltPred = IFFT.ar(PV_MagAbove(FFT(bufPred, pred), thresh))
      val fltSucc = IFFT.ar(PV_MagBelow(FFT(bufSucc, succ), thresh))
      val z = fltPred + fltSucc
      mkBlend(pred, succ, z, fade, FFTSize / SampleRate.ir)
    }

    def t4(): Unit = mkTransition { (pred, succ, fade) =>
      val thresh  = fade.linexp(1, 0, 1.0e-3, 1.0e1)
      val bufPred = LocalBuf(FFTSize)
      val bufSucc = LocalBuf(FFTSize)
      val fltPred = IFFT.ar(PV_MagBelow(FFT(bufPred, pred), thresh))
      val fltSucc = IFFT.ar(PV_MagAbove(FFT(bufSucc, succ), thresh))
      val z = fltPred + fltSucc
      mkBlend(pred, succ, z, fade, FFTSize / SampleRate.ir)
    }

    def t5(): Unit = mkTransition { (pred, succ, fade) =>
      val f1   =   10
      val f2   = 2000

      val dustFreqS = fade.linexp(0, 1, f1, f2)
      val dustFreqP = fade.linexp(1, 0, f1, f2)

      val decayTime = 0.01
      val dustS = Decay.ar(Dust.ar(dustFreqS), decayTime).min(1)
      val dustP = Decay.ar(Dust.ar(dustFreqP), decayTime).min(1)

      val fltSucc = succ * dustS
      val fltPred = pred * dustP

      //      val fadeIn = Line.kr(0, 1, dur = 2)
      //      val sig = in * (1 - fadeIn) + mod * fadeIn
      val z = fltSucc + fltPred

      mkBlend(pred, succ, z, fade, 0)
    }

    // transition 6: shift upwards
    def t6(): Unit = mkTransition { (pred, succ, fade) =>
      val freq = fade.linexp(1, 0, 22.05, 22050) - 22.05

      val fltSucc = FreqShift.ar(LPF.ar(succ, 22050 - freq),  freq)
      val fltPred = FreqShift.ar(HPF.ar(pred, 22050 - freq), -freq)

      val z = fltSucc + fltPred
      mkBlend(pred, succ, z, fade, 0)
    }

    // transition 6: shift upwards - steps
    def t6b(): Unit = mkTransition { (pred, succ, fade) =>
      val numSteps = 10
      val x        = fade * numSteps
      val fl       = Lag.kr(x.floor)
      val cl       = Lag.kr(x.ceil)
      val flFreq   = fl.linexp(numSteps, 0, 22.05, 22050) - 22.05
      val clFreq   = cl.linexp(numSteps, 0, 22.05, 22050) - 22.05
      val freq: GE = Seq(flFreq, clFreq)
    
      // val freq = fade.linexp(1, 0, 22.05, 22050) - 22.05

      val fltSucc = FreqShift.ar(LPF.ar(succ, 22050 - freq),  freq)
      val fltPred = FreqShift.ar(HPF.ar(pred, 22050 - freq), -freq)

      val z0  = fltSucc + fltPred
      val zig = x % 1
      val z   = zig * (z0 \ 1 /* aka ceil */) + (1 - zig) * (z0 \ 0 /* aka floor */)
      
      mkBlend(pred, succ, z, fade, 0)
    }

    // transition 6: shift upwards - steps
    def t6c(): Unit = mkTransition { (pred, succ, fade) =>
      val numSteps = 16 // 10
      val x        = fade * numSteps
      val xh       = x / 2
      val a        = (xh + 0.5).floor        * 2
      val b        = (xh       .floor + 0.5) * 2
      val ny       = 20000 // 22050
      val aFreq    = a.linexp(numSteps, 0, 22.05, ny) - 22.05
      val bFreq    = b.linexp(numSteps, 0, 22.05, ny) - 22.05
      // val aFreq    = a.linlin(numSteps, 0, 22.05, 22050) - 22.05
      // val bFreq    = b.linlin(numSteps, 0, 22.05, 22050) - 22.05
      val freq: GE = Seq(aFreq, bFreq)
    
      // val freq = fade.linexp(1, 0, 22.05, 22050) - 22.05

      val fltSucc = FreqShift.ar(LPF.ar(succ, ny - freq),  freq)
      val fltPred = FreqShift.ar(HPF.ar(pred, ny - freq), -freq)

      //       val z0  = fltSucc + fltPred
      //       val zig = x.fold(0, 1)
      //       val z   = zig * (z0 \ 1 /* aka ceil */) + (1 - zig) * (z0 \ 0 /* aka floor */)
      
      val z0  = fltSucc + fltPred
      val zig = x.fold(0, 1)
      val az  = zig       // .sqrt
      val bz  = (1 - zig) // .sqrt
      val z   = az * (z0 \ 1 /* aka ceil */) + bz * (z0 \ 0 /* aka floor */)
      
      //       x  .poll(2, "x")
      //       a  .poll(2, "a")
      //       b  .poll(2, "b")
      //       zig.poll(2, "zig")
      
      mkBlend(pred, succ, z, fade, 0)
    }

  
val pred = Bus.audio(s, 1)
val succ = Bus.audio(s, 1)


val x = play {
  val sig = BPZ2.ar(WhiteNoise.ar(LFPulse.kr(LFPulse.kr(0.09, 0, 0.16).madd(10, 7), 0, 0.25) * 0.1))
  Out.ar(pred.index, sig)
}

val y = play {
    val z = RLPF.ar(
    Pulse.ar(
      SinOsc.kr(4).madd(1, 80).max(
        Decay.ar(LFPulse.ar(0.1, 0, 0.05) * Impulse.ar(8) * 500, 2)
      ), 
      LFNoise1.kr(0.157).madd(0.4, 0.5)
    ) * 0.04,
    LFNoise1.kr(0.2).madd(2000, 2400),
    0.2
  )
  val y = z * 0.6
  val sig = z + Seq(
    CombL.ar(y, 0.06, LFNoise1.kr(Rand(0, 0.3)).madd(0.025, 0.035), 1) 
  + CombL.ar(y, 0.06, LFNoise1.kr(Rand(0, 0.3)).madd(0.025, 0.035), 1),
    CombL.ar(y, 0.06, LFNoise1.kr(Rand(0, 0.3)).madd(0.025, 0.035), 1)
  + CombL.ar(y, 0.06, LFNoise1.kr(Rand(0, 0.3)).madd(0.025, 0.035), 1)
  )
  
  Out.ar(succ.index, sig)
}

SynthDef.recv("foo") {
  t6c()
}

val t = Synth.play("foo", args = Seq("pred" -> pred.index, "succ" -> succ.index, "state" -> 2),
   addAction = addToTail)
   
t.free()

// val vr    = Var("blob")
// val dt    = Dim(vr, "time")
// val speed = UserValue("speed", 1).kr
// val time  = dt.play(speed)
// val data  = vr.play(time)

val x = play {
  val speed  = "speed".kr(0.5)
  val period = speed.reciprocal
  
  def voice(state: GE, second: Boolean): GE = {
    // val change = state < 2  // 0 - free, 1 - born
    val born  = state sig_== 1
    val dies  = state sig_!= 2
    
    val open  = PulseDivider.ar(born, div = 2, start = if (second) 1 else 0)
    val close = PulseDivider.ar(dies, div = 2, start = if (second) 0 else 1)
    val gate  = SetResetFF.ar(open, close)
    
    val env   = Env.asr(attack = period, release = period, curve = Curve.linear)
    val eg    = EnvGen.ar(env, gate = gate)
    
    val pan   = if (second) -1 else 1
    
    val in = PinkNoise.ar(eg * 0.2)
    
    Pan2.ar(in, pan)
  }

  val state = "state".ar(0)
      
  voice(state, false) + voice(state, true)
}

x.set("state" -> 1)

/////////////// forget it - go for 1 voice == 1 blob

val x = play {
  val gate0     = "gate".ar(0)
  val speed     = "speed".kr(0.5)
  val period    = speed.reciprocal
  val time      = Impulse.ar(speed)
  val freq0     = "freq".ar(441)
  
  // this is only to simulate the timer and "delay" control changes
  // to fall onto the raster
  val freq      = Latch.ar(freq0, time)
  val gate      = Latch.ar(gate0, time)
  
  val dFreq0    = freq absdif Delay1.ar(freq)
  val dFreq     = Latch.ar(dFreq0, dFreq0 > 0)
  val slewRate  = dFreq * speed   // makes Slew work like Ramp
  val freqR     = Slew.ar(freq, slewRate, slewRate)
  val env       = Env.asr(attack = period, release = period, curve = Curve.linear)
  val eg        = EnvGen.ar(env, gate = gate)
  val gateTr    = Trig.ar(gate, SampleDur.ir)
  val toggle    = gateTr + TDelay.ar(gateTr, period)
  val ff        = ToggleFF.ar(toggle)
  val freq1     = Select.ar(ff, Seq(freqR, freq))
  val sig       = SinOsc.ar(freq1) * eg * 0.2
  
  // freqR.poll(2, "freqR")
  // slewRate.poll(2, "slew")
  val frame     = PulseCount.ar(time)
  frame.poll(time, "frame")
  
  Pan2.ar(sig)
}

// no glissando during fade-in
x.set("gate" -> 1, "freq" -> 1000)
// subsequent ramps
x.set("freq" -> 441)
x.set("freq" -> 881)
x.set("gate" -> 0)
// again, no gliss
x.set("gate" -> 1, "freq" -> 333)
x.set("freq" -> 881)



LinXFade2.ar(inA, inB, LFTri.ar(speed/2, iphase))





/*
val amp: GE = ???
val keyVal: GE = "key".kr
val res: GE = If (keyVal > 0) {
  WhiteNoise.ar(amp)
} Else If (keyVal > 1) {
  SinOsc.ar(441) * amp
} Else {
  DC.ar(0)
}
Out.ar(0, res)
*/

SynthDef.recv("main") {
  val branch  = "branch".ir
  val attr    = "attr".kr
  val amp     = "amp".kr
  val ampBus  = "ampBus".kr
  Out.ar(ampBus, amp)
  val resBus  = 
}

SynthDef.recv("if") {
  val ampBus = "ampBus".kr
  val amp = In.kr(ampBus)
  val res = WhiteNoise.ar(amp)
  val resBus = "resBus".kr 
  Out.ar(resBus, res)
}

SynthDef.recv("elseif") {
  val ampBus = "ampBus".kr
  val amp = In.kr(ampBus)
  val res = SinOsc.ar(441) * amp
  val resBus = "resBus".kr 
  Out.ar(resBus, res)
}

SynthDef.recv("else") {
  val res = DC.ar(0)
  val resBus = "resBus".kr 
  Out.ar(resBus, res)
}

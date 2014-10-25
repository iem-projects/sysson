// state: either of 2 (fade-in) or 3 (fade-out)

SynthDef.recv("eg") {
  val state = "state".kr(2)
  val gate  = 3 - state // 1 for fade-in, 0 for fade-out
  val atk   = 10.0
  val rls   = 10.0
  val env   = Env.asr(attack = atk, release = rls, curve = Curve.lin)
  val fade  = EnvGen.kr(env, gate = gate, doneAction = freeSelf)
  fade.poll()
}

val in  = Synth.play("eg", Seq("state" -> 2))
val out = Synth.play("eg", Seq("state" -> 3))

SynthDef.recv("slew") {
  val state  = "state".kr(2)
  val target = 3 - state // 1 for fade-in, 0 for fade-out
  val start  = 1 - target
  val atk    = 10.0
  val rls    = 10.0
  val in     = Select.kr(ToggleFF.kr(1), Seq(start, target))
  val fade   = Slew.kr(in, atk.reciprocal, rls.reciprocal)
  val done   = fade sig_== target
  FreeSelf.kr(done)
  fade.poll()
}

val in  = Synth.play("slew", Seq("state" -> 2))
in.set("state" -> 3)  // invert direction


val out = Synth.play("slew", Seq("state" -> 3))


play {
  val in = DC.kr(1)
  Slew.kr(in, 0.1, 0.1).poll()
  ()
}

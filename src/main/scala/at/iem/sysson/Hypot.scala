package at.iem.sysson

/*

  val latRange  = UserSelectRange(Latitude)
  val timeRange = UserSelectRange(Time)
  val plev      = UserSelectValue(Pressure)
  val speed     = UserRotary(speedSpec)   // --> position, label etc. via view-map ?

  // val sel       = VarSelect(VarSelect(VarSelect(latRange), timeRange), plev)
  val sel       = Var.select(latRange).select(timeRange).select(plev)

  // sel.ar(timeRange)

  sel.ir

  val time      = timeRange.ar(freq)
  val sig       = sel.ar(time)

  Pan.ar(SinOsc.ar(sig), sig(Latitude).linlin(latRange.min, latRange.max, -1, 1))

*/
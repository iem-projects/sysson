val f = openFile(userHome / "IEM" / "SysSon" / "installation" / "data" / "ta_anom_spk2.nc")
val sec = f.variableMap("Temperature") in "Time" select (12 to 144)
val x = sec.read().double1D
x.min
x.max
x.exists(_.isNaN)


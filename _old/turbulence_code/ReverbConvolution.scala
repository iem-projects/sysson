val path = "/home/hhrutz/IEM/SysSon/installation/audio_work/ForumVerb"

val irSize = 80000
val fullL = Buffer.alloc(s, irSize)
val fullR = Buffer.alloc(s, irSize)

fullL.read(path + "-L.aif")
fullR.read(path + "-R.aif")

val fftSize = 2048
val numPart  = (irSize * 2.0 / fftSize).ceil.toInt  // 79
val partSize = fftSize * numPart  // 161792
val partL = Buffer.alloc(s, partSize)
val partR = Buffer.alloc(s, partSize)

s ! osc.Message("/b_gen", partL.id, "PreparePartConv", fullL.id, fftSize)
s ! osc.Message("/b_gen", partR.id, "PreparePartConv", fullR.id, fftSize)

play {
  val mix   = Dust.ar(10) * "amp".kr(1)
  val convL = PartConv.ar(mix, fftSize, partL.id)
  val convR = PartConv.ar(mix, fftSize, partR.id)
  Out.ar(0, Seq(convL, convR))
}


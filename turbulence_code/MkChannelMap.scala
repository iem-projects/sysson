import at.iem.sysson.turbulence._
val m = Turbulence.MatrixToChannelMap
val f = userHome / "sysson" / "buffers" / "channel-map.aif"
val af = io.AudioFile.openWrite(f, io.AudioFileSpec(
  numChannels = 1, numFrames = 14 * 5, sampleRate = 44100)
)
val b = Array.tabulate(14)(x => Array.tabulate(5)(y =>
  m.getOrElse((x, y), 0).toFloat
)).flatten
af.write(Array(b))
af.close()


import at.iem.sysson.turbulence._
val f = userHome / "sysson" / "buffers" / "latlon-map.aif"

def m(latIdx: Int, lonIdx: Int) = {
  val lat = latIdx.linlin(0,  72,    -90,  90)
  val lon = lonIdx.linlin(0, 143, -177.5, 180)
  Dymaxion.mapLonLat(lon, lat)
}

val b = Array.tabulate(73) { latIdx => 
  Array.tabulate(144) { lonIdx =>
    val pt = m(latIdx, lonIdx)
    Array(pt.x.toFloat, pt.y.toFloat)
  } .flatten
} .flatten

val af = io.AudioFile.openWrite(f, io.AudioFileSpec(
  numChannels = 1, numFrames = 73 * 144 * 2, sampleRate = 44100)
)

af.write(Array(b))
af.close()


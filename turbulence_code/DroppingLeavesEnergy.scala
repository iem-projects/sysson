import osc.Implicits._
val c = osc.UDP.Client(localhost -> 17737)

var marks = Vector.empty[Long]

c.action = {
  case osc.Message("/query.reply", 1, count: Int) =>
    c ! osc.Message("/doc/active/markers", "get", 2, "range", 0, count)
  case osc.Message("/get.reply", 2, info @ _*) =>
    marks = info.sliding(1, 2).flatten.collect {
      case p: Int => p.toLong
    } .toVector
    println(marks.size)
  case other => println(s"Not handled: $other")
}

c.connect()

c ! osc.Message("/doc/active/markers", "query", 1, "count")

marks // result

//////////////////

val pathIn = "/home/hhrutz/IEM/SysSon/installation/audio_work/DroppingLeaves1EdHPF.aif"

val lens    = marks.differentiate.map(_.toInt)

def calcEnergy(): Vector[Double] = {
  val af = io.AudioFile.openRead(pathIn)
  try {
    val bufSize = lens.max
    val buf     = af.buffer(bufSize)
    lens.map { len =>
      af.read(buf, 0, len)
      val sqr = buf.flatMap { ch =>
        ch.take(len).map(_.squared.toDouble)
      }
      (sqr.sum / sqr.size).sqrt
    }
    
  } finally {
    af.close()
  }
}

val en = calcEnergy()
en.size == marks.size - 1 // true

val indices = en.zipWithIndex.sortBy(_._1).map(_._2)

val pathOut = "/home/hhrutz/IEM/SysSon/installation/audio_work/DroppingLeaves1Sort.aif"

def writeSorted(): Unit = {
  val afIn = io.AudioFile.openRead(pathIn)
  try {
    val bufSize = lens.max
    val buf     = afIn.buffer(bufSize)
    val afOut   = io.AudioFile.openWrite(pathOut, afIn.spec)
    try {
      indices.foreach { idx =>
        val start = marks(idx)
        val stop  = marks(idx + 1)
        val len   = (stop - start).toInt
        afIn.seek(start)
        afIn.read(buf, 0, len)
        afOut.write(buf, 0, len)
      }
    } finally {
      afOut.close()
    } 
  } finally {
    afIn.close()
  }
}

writeSorted()

val sorted = 0 +: indices.map(lens.apply).integrate
// sorted.foreach(println)
sorted.size // 140

val pathMark = "/home/hhrutz/IEM/SysSon/installation/audio_work/DroppingLeaves1Mark.aif"

def writeMarks(): Unit = {
  val bufSorted = Array(sorted.map(_.toFloat).toArray)
  val afMark = io.AudioFile.openWrite(pathMark, 
    io.AudioFileSpec(numChannels = 1, numFrames = pathMark.size, sampleRate = 44100))
  try {
    afMark.write(bufSorted)
  } finally {
    afMark.close()
  }
}

writeMarks()

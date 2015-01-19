val base = userHome / "Documents" / "devel" / "sysson"
val io = new java.io.FileInputStream(base / "_creation" / "BlankMap-Equirectangular_union.txt")
val arr = new Array[Byte](io.available)
io.read(arr)
io.close()
val path = new String(arr, "UTF-8")
path.toSet  // M, L, z
path.count(_ == 'M') // 1430
path.count(_ == 'z') // 1430

val resBase = base / "src" / "main" / "resources" / "at" / "iem" / "sysson"
assert(resBase.isDirectory)

val resF = resBase / "BlankMap-Equirectangular.dat"
val segm = path.filterNot(c => c == 'L' || c == 'z').split('M').map(_.trim).filter(_.nonEmpty)
val segmP: Array[Vector[(Float, Float)]] = segm.map { s => 
  val t = s.split(' ').map(_.trim).filter(_.nonEmpty).map(_.toDouble.toFloat).toVector
  val g = t.grouped(2).toVector.map { case Seq(x, y) => (x, y) }
  g.sliding(1, 2).toVector.flatten // skip every second point for coarser resolution
}
segmP.map(_.size).max

val raf = new java.io.RandomAccessFile(resF, "rw")
raf.writeShort(segmP.size)
segmP.foreach { s =>
  raf.writeShort(s.size)
  s.foreach { case (x,y) => raf.writeFloat(x); raf.writeFloat(y) }
}
raf.close()
resF.length  // 240K

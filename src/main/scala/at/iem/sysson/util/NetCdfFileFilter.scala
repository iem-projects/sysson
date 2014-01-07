package at.iem.sysson.util

import de.sciss.file.File
import java.io.RandomAccessFile
import scala.util.control.NonFatal

object NetCdfFileFilter extends (File => Boolean) {
  def apply(f: File): Boolean = try {
    // NOTE: NetcdfFile.canOpen is really badly written, very slow. Therefore,
    // make a short cut and just check for NetCDF cookies
    // NetcdfFile.canOpen(f.getPath)
    val r = new RandomAccessFile(f, "r")
    try {
      if (f.length() < 4) false else {
        val cookie = r.readInt()
        cookie == 0x43444601 || cookie == 0x43444602
      }
    } finally {
      r.close()
    }
  } catch {
    case NonFatal(_) => false
  }
}

package at.iem.sysson.graph

import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}

object Dim {
  private final val DIM_COOKIE = 0x64696D00 // "dim\0"

  implicit object serializer extends ImmutableSerializer[Dim] {
    def read(in: DataInput): Dim = {
      val cookie = in.readInt()
      require(cookie == DIM_COOKIE,
        s"Unexpected cookie (expected ${DIM_COOKIE.toHexString}, found ${cookie.toHexString})")
      val name    = in.readUTF()
      val minSize = ImmutableSerializer.option[Int].read(in)
      val maxSize = ImmutableSerializer.option[Int].read(in)
      Dim(name, minSize = minSize, maxSize = maxSize)
    }

    def write(dim: Dim, out: DataOutput): Unit = {
      out.writeInt(DIM_COOKIE)
      out.writeUTF(dim.name)
      ImmutableSerializer.option[Int].write(dim.minSize, out)
      ImmutableSerializer.option[Int].write(dim.maxSize, out)
    }
  }
}
/** Specification of a data source dimension
  *
  * @param name     Logical name by which the dimension is referred to
  * @param minSize  Minimum domain size, or none
  * @param maxSize  Maximum domain size, or none
  */
case class Dim(name: String, minSize: Option[Int], maxSize: Option[Int])

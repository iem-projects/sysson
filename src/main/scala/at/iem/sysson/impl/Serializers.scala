package at.iem.sysson.impl

import de.sciss.serial.{DataOutput, DataInput, ImmutableSerializer}

object Serializers {
  implicit object RangeSerializer extends ImmutableSerializer[Range] {
    def read(in: DataInput): Range = {
      val start       = in readInt()
      val end         = in readInt()
      val isInclusive = in readBoolean()
      val step        = in readInt()
      if (isInclusive)
        Range.inclusive(start, end, step)
      else
        Range.apply    (start, end, step)
    }

    def write(r: Range, out: DataOutput): Unit = {
      import r._
      out writeInt     start
      out write        end
      out writeBoolean isInclusive
      out writeInt     step
    }
  }
}

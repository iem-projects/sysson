package at.iem.sysson
package sound

import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}

object SonificationSpec {
  private final val SPEC_COOKIE = 0x737973736F6E7300L  // "syssons\0"

  implicit object serializer extends ImmutableSerializer[SonificationSpec] {
    def read(in: DataInput): SonificationSpec = {
      val cookie = in.readLong()
      require(cookie == SPEC_COOKIE,
        s"Unexpected cookie (expected ${SPEC_COOKIE.toHexString}, found ${cookie.toHexString}")

      val sources = ImmutableSerializer.indexedSeq[graph.Var      ].read(in)
      val user    = ImmutableSerializer.indexedSeq[graph.UserValue].read(in)
      SonificationSpec(sources, user)
    }

    def write(spec: SonificationSpec, out: DataOutput): Unit = {
      out.writeLong(SPEC_COOKIE)
      ImmutableSerializer.indexedSeq[graph.Var      ].write(spec.sources, out)
      ImmutableSerializer.indexedSeq[graph.UserValue].write(spec.user   , out)
    }
  }
}
case class SonificationSpec(sources: Vec[graph.Var] = Vec.empty, user: Vec[graph.UserValue] = Vec.empty)

package at.iem.sysson.graph

import de.sciss.synth.{proc, Rate, scalar, GE, UGenInLike}
import de.sciss.synth
import at.iem.sysson.sound.AuralSonification

object UserValue {

  //  private final val USER_COOKIE = 0x75737200  // "usr\0"
  //
  //  implicit object serializer extends ImmutableSerializer[UserValue] {
  //    def read(in: DataInput): UserValue = {
  //      val cookie = in.readInt()
  //      require(cookie == USER_COOKIE,
  //        s"Unexpected cookie (expected ${USER_COOKIE.toHexString}, found ${cookie.toHexString})")
  //      val key     = in.readUTF()
  //      val default = in.readDouble()
  //      UserValue(key, default)
  //    }
  //
  //    def write(v: UserValue, out: DataOutput): Unit = {
  //      out.writeInt(USER_COOKIE)
  //      out.writeUTF(v.key)
  //      out.writeDouble(v.default)
  //    }
  //  }

  // XXX TODO: shouldn't leak impl.LazyImpl
  case class GE(rate: Rate, peer: UserValue) extends synth.GE.Lazy {
    override def productPrefix = "UserValue$GE"

    // protected def makeUGens(b: UGenGraphBuilderOLD): UGenInLike =
    //  b.addScalarUserValue(peer)

    protected def makeUGens: UGenInLike = {
      val key = AuralSonification.current().attributeKey(this)
      proc.graph.attribute(key).ir
    }

    // proc.graph.attribute(peer.attrKey).ir(peer.default)
  }
}
case class UserValue(key: String, default: Double) extends UserInteraction {
  def value: GE = UserValue.GE(scalar , this)
  def ir   : GE = UserValue.GE(scalar , this)
  // def kr: GE = UserValue.GE(control, this)
  // def ar: GE = UserValue.GE(audio  , this)

  // private[sysson] def attrKey = s"user_$key"
}
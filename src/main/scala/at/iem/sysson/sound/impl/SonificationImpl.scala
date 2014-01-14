package at.iem.sysson
package sound
package impl

import de.sciss.lucre.{event => evt, expr}
import de.sciss.lucre.event.{InMemory, Event, Sys}
import at.iem.sysson.sound.Sonification.AttributeKey
import de.sciss.synth.proc.Attribute
import scala.annotation.switch
import de.sciss.serial.{DataInput, DataOutput}
import de.sciss.lucre.data.SkipList

object SonificationImpl {
  private final val SER_VERSION = 0x536f6e00  // "Son\0"

  def apply[S <: Sys[S]](implicit tx: S#Tx): Sonification[S] = new New[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Sonification[S] =
    serializer[S].read(in, access)

  def serializer[S <: Sys[S]]: evt.NodeSerializer[S, Sonification[S]] =
    anySer.asInstanceOf[evt.NodeSerializer[S, Sonification[S]]]

  private val anySer = new Serializer[InMemory]

  private class Serializer[S <: Sys[S]] extends evt.NodeSerializer[S, Sonification[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Sonification[S] =
      new Read[S](in, access, targets)
  }

  private sealed trait Impl[S <: Sys[S]] extends Sonification[S] with HasAttributes[S, Sonification[S]] {
    sonif =>

    type Update       = Sonification.Update[S]
    type Change       = Sonification.Change[S]

    final protected def reader: evt.Reader[S, Sonification[S]] = SonificationImpl.serializer

    final protected def AssociationAdded  (key: String) = Sonification.AssociationAdded  [S](AttributeKey(key))
    final protected def AssociationRemoved(key: String) = Sonification.AssociationRemoved[S](AttributeKey(key))
    final protected def AttributeChange   (key: String, u: Attribute.Update[S]) =
      Sonification.AttributeChange(key, u.element, u.change)

    final protected def Update(changes: Vec[Change]) = Sonification.Update(sonif, changes)

    object changed
      extends evt.impl.EventImpl[S, Update, Sonification[S]]
      with evt.InvariantEvent   [S, Update, Sonification[S]]
      with SelfEvent {

      final val slot = 3

      def connect   ()(implicit tx: S#Tx): Unit = {
        patch.changed ---> this
        attributes    ---> this
        StateEvent    ---> this
      }
      def disconnect()(implicit tx: S#Tx): Unit = {
        patch.changed -/-> this
        attributes    -/-> this
        StateEvent    -/-> this
      }

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Update] = {
        // val patchOpt = if (graphemes .isSource(pull)) graphemes .pullUpdate(pull) else None
        val patchCh  = patch.changed
        val patchOpt = if (pull.contains(patchCh   )) pull(patchCh   ) else None
        val attrOpt  = if (pull.contains(attributes)) pull(attributes) else None
        val stateOpt = if (pull.contains(StateEvent)) pull(StateEvent) else None

        val seq0 = patchOpt.fold(Vec.empty[Change]) { u =>
          u.changes.map(Sonification.PatchChange(_))
        }
        val seq1 = attrOpt.fold(seq0) { u =>
          if (seq0.isEmpty) u.changes else seq0 ++ u.changes
        }
        val seq3 = stateOpt.fold(seq1) { u =>
          if (seq1.isEmpty) u.changes else seq1 ++ u.changes
        }
        if (seq3.isEmpty) None else Some(Update(seq3))
      }
    }

    final def select(slot: Int): Event[S, Any, Any] = (slot: @switch) match {
      case changed    .slot => changed
      case attributes .slot => attributes
      case StateEvent .slot => StateEvent
    }

    final protected def writeData(out: DataOutput): Unit = {
      out.writeInt(SER_VERSION)
      patch       .write(out)
      attributeMap.write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      patch       .dispose()
      attributeMap.dispose()
    }

    override def toString() = "Sonification" + id
  }

  import HasAttributes.attributeEntryInfo

  private final class New[S <: Sys[S]](implicit tx0: S#Tx) extends Impl[S] {
    protected val targets       = evt.Targets[S](tx0)
    val patch                   = Patch[S]
    val sources                 = expr.Map.Modifiable[S, String, Sonification.Source[S], Sonification.Source.Update[S]]
    protected val attributeMap  = SkipList.Map.empty[S, String, AttributeEntry]
  }

  private final class Read[S <: Sys[S]](in: DataInput, access: S#Acc, protected val targets: evt.Targets[S])
                                       (implicit tx0: S#Tx)
    extends Impl[S] {

    {
      val serVer = in.readInt()
      require(serVer == SER_VERSION, s"Incompatible serialized (found $serVer, required $SER_VERSION)")
    }

    val patch                   = Patch.read(in, access)
    val sources                 = expr.Map.read[S, String, Sonification.Source[S], Sonification.Source.Update[S]](in, access)
    protected val attributeMap  = SkipList.Map.read[S, String, AttributeEntry](in, access)
  }
}

package at.iem.sysson
package sound
package impl

import de.sciss.lucre.data.SkipList
import de.sciss.lucre.{event => evt}
import evt.{InMemory, Sys, Event}
import de.sciss.synth.proc.impl.KeyMapImpl
import de.sciss.serial.{DataOutput, DataInput, ImmutableSerializer}
import de.sciss.synth.proc.{SynthGraphs, Attributes, Attribute}
import de.sciss.synth.proc
import scala.annotation.switch
import scala.collection.breakOut
import at.iem.sysson.sound.Patch.AttributeKey
import language.higherKinds

object PatchImpl {
  private final val SER_VERSION = 0x50617400  // "Pat\0"

  def apply[S <: Sys[S]](implicit tx: S#Tx): Patch[S] = new New[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Patch[S] =
    serializer[S].read(in, access)

  def serializer[S <: Sys[S]]: evt.NodeSerializer[S, Patch[S]] =
    anySer.asInstanceOf[evt.NodeSerializer[S, Patch[S]]]

  private val anySer = new Serializer[InMemory]

  private class Serializer[S <: Sys[S]] extends evt.NodeSerializer[S, Patch[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Patch[S] =
      new Read[S](in, access, targets)
  }

  private type AttributeEntry[S <: Sys[S]] = KeyMapImpl.Entry[S, String, Attribute[S], Attribute.Update[S]]

  private type I = InMemory

  implicit def attributeEntryInfo[S <: Sys[S]]: KeyMapImpl.ValueInfo[S, String, Attribute[S], Attribute.Update[S]] =
    anyAttributeEntryInfo.asInstanceOf[KeyMapImpl.ValueInfo[S, String, Attribute[S], Attribute.Update[S]]]

  private val anyAttributeEntryInfo = new KeyMapImpl.ValueInfo[I, String, Attribute[I], Attribute.Update[I]] {
    def valueEvent(value: Attribute[I]) = value.changed

    val keySerializer   = ImmutableSerializer.String
    val valueSerializer = Attribute.serializer[I]
  }

  private sealed trait Impl[S <: Sys[S]] extends Patch[S] {
    patch =>

    protected def attributeMap: SkipList.Map[S, String, AttributeEntry[S]]

    // ---- key maps ----

    sealed trait PatchEvent {
      final protected def reader: evt.Reader[S, Patch[S]] = PatchImpl.serializer
      final def node: Patch[S] with evt.Node[S] = patch
    }

    sealed trait KeyMap[Value, ValueUpd, OuterUpd]
      extends evt.impl.EventImpl [S, OuterUpd, Patch[S]]
      with evt.InvariantEvent    [S, OuterUpd, Patch[S]]
      with PatchEvent
      with proc.impl.KeyMapImpl[S, String, Value, ValueUpd] {
      protected def wrapKey(key: String): Patch.AssociativeKey

      // ---- keymapimpl details ----

      final protected def fire(added: Set[String], removed: Set[String])(implicit tx: S#Tx): Unit = {
        val seqAdd: Vec[Patch.StateChange[S]] = added  .map(key => Patch.AssociationAdded  [S](wrapKey(key)))(breakOut)
        val seqRem: Vec[Patch.StateChange[S]] = removed.map(key => Patch.AssociationRemoved[S](wrapKey(key)))(breakOut)
        // convention: first the removals, then the additions. thus, overwriting a key yields
        // successive removal and addition of the same key.
        val seq = if (seqAdd.isEmpty) seqRem else if (seqRem.isEmpty) seqAdd else seqRem ++ seqAdd

        StateEvent(Patch.Update(patch, seq))
      }

      final protected def isConnected(implicit tx: S#Tx): Boolean = patch.targets.nonEmpty
    }

    object attributes extends Attributes.Modifiable[S] with KeyMap[Attribute[S], Attribute.Update[S], Patch.Update[S]] {
      final val slot = 0

      protected def wrapKey(key: String) = AttributeKey(key)

      def put(key: String, value: Attribute[S])(implicit tx: S#Tx): Unit = add(key, value)

      def contains(key: String)(implicit tx: S#Tx): Boolean = map.contains(key)

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Patch.Update[S]] = {
        val changes = foldUpdate(pull)
        if (changes.isEmpty) None
        else Some(Patch.Update(patch,
          changes.map({
            case (key, u) => Patch.AttributeChange(key, u.element, u.change)
          })(breakOut)))
      }

      protected def map: SkipList.Map[S, String, Entry] = attributeMap

      protected def valueInfo = attributeEntryInfo[S]

      def apply[Attr[~ <: Sys[~]] <: Attribute[_]](key: String)(implicit tx: S#Tx,
                                                                tag: reflect.ClassTag[Attr[S]]): Option[Attr[S]#Peer] =
        get(key) match {
          // cf. stackoverflow #16377741
          case Some(attr) => tag.unapply(attr).map(_.peer) // Some(attr.peer)
          case _          => None
        }
    }

    private object StateEvent
      extends evt.impl.TriggerImpl[S, Patch.Update[S], Patch[S]]
      with evt.InvariantEvent     [S, Patch.Update[S], Patch[S]]
      with evt.impl.Root          [S, Patch.Update[S]]
      with PatchEvent {

      final val slot = 2
    }

    object changed
      extends evt.impl.EventImpl[S, Patch.Update[S], Patch[S]]
      with evt.InvariantEvent   [S, Patch.Update[S], Patch[S]]
      with PatchEvent {

      final val slot = 3

      def connect   ()(implicit tx: S#Tx): Unit = {
        graph.changed ---> this
        attributes    ---> this
        StateEvent    ---> this
      }
      def disconnect()(implicit tx: S#Tx): Unit = {
        graph.changed -/-> this
        attributes    -/-> this
        StateEvent    -/-> this
      }

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Patch.Update[S]] = {
        // val graphOpt = if (graphemes .isSource(pull)) graphemes .pullUpdate(pull) else None
        val graphCh  = graph.changed
        val graphOpt = if (pull.contains(graphCh   )) pull(graphCh   ) else None
        val attrOpt  = if (pull.contains(attributes)) pull(attributes) else None
        val stateOpt = if (pull.contains(StateEvent)) pull(StateEvent) else None

        val seq0 = graphOpt.fold(Vec.empty[Patch.Change[S]]) { u =>
          Vec(Patch.GraphChange(u))
        }
        val seq1 = attrOpt.fold(seq0) { u =>
          if (seq0.isEmpty) u.changes else seq0 ++ u.changes
        }
        val seq3 = stateOpt.fold(seq1) { u =>
          if (seq1.isEmpty) u.changes else seq1 ++ u.changes
        }
        if (seq3.isEmpty) None else Some(Patch.Update(patch, seq3))
      }
    }

    final def select(slot: Int): Event[S, Any, Any] = (slot: @switch) match {
      case changed    .slot => changed
      case attributes .slot => attributes
      case StateEvent .slot => StateEvent
    }

    final protected def writeData(out: DataOutput): Unit = {
      out.writeInt(SER_VERSION)
      graph       .write(out)
      attributeMap.write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      graph       .dispose()
      attributeMap.dispose()
    }

    override def toString() = "Patch" + id
  }

  private final class New[S <: Sys[S]](implicit tx0: S#Tx) extends Impl[S] {
    protected val targets       = evt.Targets[S](tx0)
    val graph                   = SynthGraphs.newVar(SynthGraphs.empty)
    protected val attributeMap  = SkipList.Map.empty[S, String, AttributeEntry[S]]
  }

  private final class Read[S <: Sys[S]](in: DataInput, access: S#Acc, protected val targets: evt.Targets[S])
                                       (implicit tx0: S#Tx)
    extends Impl[S] {

    {
      val serVer = in.readInt()
      require(serVer == SER_VERSION, s"Incompatible serialized (found $serVer, required $SER_VERSION)")
    }

    val graph                   = SynthGraphs.readVar(in, access)
    protected val attributeMap  = SkipList.Map.read[S, String, AttributeEntry[S]](in, access)
  }
}

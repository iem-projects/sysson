/*
 *  HasAttributes.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson.sound.impl

import de.sciss.lucre.{event => evt}
import de.sciss.lucre.event.{InMemory, Sys}
import de.sciss.lucre.data.SkipList
import de.sciss.synth.proc.impl.KeyMapImpl
import de.sciss.synth.proc.{Attr, AttrMap}
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.breakOut
import de.sciss.synth.proc
import language.higherKinds
import de.sciss.serial.ImmutableSerializer

object HasAttributes {
  implicit def attributeEntryInfo[S <: Sys[S]]: KeyMapImpl.ValueInfo[S, String, Attr[S], Attr.Update[S]] =
    anyAttributeEntryInfo.asInstanceOf[KeyMapImpl.ValueInfo[S, String, Attr[S], Attr.Update[S]]]

  private type I = InMemory

  private val anyAttributeEntryInfo = new KeyMapImpl.ValueInfo[I, String, Attr[I], Attr.Update[I]] {
    def valueEvent(value: Attr[I]) = value.changed

    val keySerializer   = ImmutableSerializer.String
    val valueSerializer = Attr.serializer[I]
  }
}
trait HasAttributes[S <: Sys[S], Repr <: evt.Node[S]] {
  self: Repr =>

  // ---- abstract ----

  protected type Update
  protected type Change

  protected def Update(changes: Vec[Change]): Update
  protected def AssociationAdded  (key: String): Change
  protected def AssociationRemoved(key: String): Change
  protected def AttributeChange   (key: String, u: Attr.Update[S]): Change

  protected def reader: evt.Reader[S, Repr]

  protected def attributeMap: SkipList.Map[S, String, AttributeEntry]

  // ---- impl ----
  import HasAttributes.attributeEntryInfo

  protected type AttributeEntry = KeyMapImpl.Entry[S, String, Attr[S], Attr.Update[S]]

  /* sealed */ protected trait SelfEvent {
    final protected def reader: evt.Reader[S, Repr] = self.reader
    final def node: Repr = self
  }

  sealed trait KeyMap[Value, ValueUpd, OuterUpd]
    extends evt.impl.EventImpl [S, OuterUpd, Repr]
    with evt.InvariantEvent    [S, OuterUpd, Repr]
    with SelfEvent
    with proc.impl.KeyMapImpl[S, String, Value, ValueUpd] {

    // ---- keymapimpl details ----

    final protected def fire(added: Set[String], removed: Set[String])(implicit tx: S#Tx): Unit = {
      val seqAdd: Vec[Change] = added  .map(key => AssociationAdded  (key))(breakOut)
      val seqRem: Vec[Change] = removed.map(key => AssociationRemoved(key))(breakOut)
      // convention: first the removals, then the additions. thus, overwriting a key yields
      // successive removal and addition of the same key.
      val seq = if (seqAdd.isEmpty) seqRem else if (seqRem.isEmpty) seqAdd else seqRem ++ seqAdd

      StateEvent(Update(seq))
    }

    final protected def isConnected(implicit tx: S#Tx): Boolean = self.targets.nonEmpty
  }

  object attributes extends AttrMap.Modifiable[S] with KeyMap[Attr[S], Attr.Update[S], Update] {
    final val slot = 0

    def put(key: String, value: Attr[S])(implicit tx: S#Tx): Unit = add(key, value)

    def contains(key: String)(implicit tx: S#Tx): Boolean = map.contains(key)

    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Update] = {
      val changes = foldUpdate(pull)
      if (changes.isEmpty) None
      else Some(Update(
        changes.map({
          case (key, u) => AttributeChange(key, u) // u.element, u.change)
        })(breakOut)))
    }

    protected def map: SkipList.Map[S, String, Entry] = attributeMap

    protected def valueInfo = attributeEntryInfo[S]

    def apply[At[~ <: Sys[~]] <: Attr[_]](key: String)(implicit tx: S#Tx,
                                                              tag: reflect.ClassTag[At[S]]): Option[At[S]#Peer] =
      get(key) match {
        // cf. stackoverflow #16377741
        case Some(attr) => tag.unapply(attr).map(_.peer) // Some(attr.peer)
        case _          => None
      }
  }

  protected object StateEvent
    extends evt.impl.TriggerImpl[S, Update, Repr]
    with evt.InvariantEvent     [S, Update, Repr]
    with evt.impl.Root          [S, Update]
    with SelfEvent {

    final val slot = 2
  }
}
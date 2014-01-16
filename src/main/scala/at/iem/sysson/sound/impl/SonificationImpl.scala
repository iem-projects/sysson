/*
 *  SonificationImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package sound
package impl

import de.sciss.lucre.{event => evt, expr}
import de.sciss.lucre.event.{Pull, EventLike, InMemory, Event, Sys}
import at.iem.sysson.sound.Sonification.Source
import de.sciss.synth.proc.Attribute
import scala.annotation.switch
import de.sciss.serial.{DataInput, DataOutput}
import de.sciss.lucre.data.SkipList
import de.sciss.lucre.expr.Expr
import de.sciss.model
import de.sciss.lucre.synth.expr.{Doubles, Strings}

object SonificationImpl {
  private final val SER_VERSION = 0x53726300  // "Src\0"

  def sourceSerializer[S <: Sys[S]]: evt.NodeSerializer[S, Source[S]] = anySourceSer.asInstanceOf[SourceSer[S]]

  private val anySourceSer = new SourceSer[evt.InMemory]

  private final class SourceSer[S <: Sys[S]] extends evt.NodeSerializer[S, Source[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Source[S] = {
      implicit val str = Strings.serializer[S]
      val cookie = in.readInt()
      require(cookie == SER_VERSION,
        s"Unexpected cookie (expected ${SER_VERSION.toHexString}, found ${cookie.toHexString})")
      val data = DataSource.read(in, access)
      val dims = expr.Map.read[S, String, Expr[S, String], model.Change[String]](in, access)
      new SourceImpl(targets, data, dims)
    }
  }

  private final class SourceImpl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                        val data: DataSource[S],
                                        val dims: expr.Map[S, String, Expr[S, String], model.Change[String]])
    extends Source[S]
    with evt.impl.StandaloneLike[S, Source.Update[S], Source[S]] {
    source =>

    def disconnect()(implicit tx: S#Tx): Unit = dims.changed ---> this
    def connect   ()(implicit tx: S#Tx): Unit = dims.changed -/-> this

    protected def disposeData()(implicit tx: S#Tx): Unit = dims.dispose()

    protected def writeData(out: DataOutput): Unit = {
      out.writeInt(SER_VERSION)
      data.write(out)
      dims.write(out)
    }

    def changed: EventLike[S, Source.Update[S]] = this

    protected def reader: evt.Reader[S, Source[S]] = sourceSerializer

    def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[Source.Update[S]] =
      pull(dims.changed).map { u =>
        Source.DimsChanged(source, u)
      }
  }

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

    final protected def AssociationAdded  (key: String) = Sonification.AttributeAdded  [S](key)
    final protected def AssociationRemoved(key: String) = Sonification.AttributeRemoved[S](key)
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

      // XXX TODO: for completeness, should forward changes to sources and controls!
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
      sources     .write(out)
      controls    .write(out)
      attributeMap.write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      patch       .dispose()
      sources     .dispose()
      controls    .dispose()
      attributeMap.dispose()
    }

    override def toString() = "Sonification" + id
  }

  import HasAttributes.attributeEntryInfo

  private final class New[S <: Sys[S]](implicit tx0: S#Tx) extends Impl[S] {
    protected val targets       = evt.Targets[S](tx0)
    val patch                   = Patch[S]
    val sources                 = expr.Map.Modifiable[S, String, Source[S], Source.Update[S]]
    val controls                = {
      implicit val ser = Doubles.serializer[S]
      expr.Map.Modifiable[S, String, Expr[S, Double], model.Change[Double]]
    }
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
    val sources                 = expr.Map.read[S, String, Source[S], Source.Update[S]](in, access)
    val controls                = {
      implicit val ser = Doubles.serializer[S]
      expr.Map.read[S, String, Expr[S, Double], model.Change[Double]](in, access)
    }
    protected val attributeMap  = SkipList.Map.read[S, String, AttributeEntry](in, access)
  }
}

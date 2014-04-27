/*
 *  PatchImpl.scala
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

package at.iem.sysson
package sound
package impl

import de.sciss.lucre.data.SkipList
import de.sciss.lucre.{event => evt}
import evt.{InMemory, Sys, Event}
import de.sciss.serial.{DataOutput, DataInput}
import de.sciss.synth.proc.{SynthGraphs, Elem}
import scala.annotation.switch
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

  private sealed trait Impl[S <: Sys[S]] extends Patch[S] {
    patch =>

    type Update       = Patch.Update[S]
    type Change       = Patch.Change[S]
    type StateChange  = Patch.StateChange[S]

    final protected def reader: evt.Reader[S, Patch[S]] = PatchImpl.serializer

    final protected def Update(changes: Vec[Change]) = Patch.Update(patch, changes)

    /* sealed */ protected trait SelfEvent {
      final protected def reader: evt.Reader[S, Patch[S]] = patch.reader
      final def node: Patch[S] = patch
    }

    object changed
      extends evt.impl.EventImpl[S, Update, Patch[S]]
      with evt.InvariantEvent   [S, Update, Patch[S]]
      with SelfEvent {

      final val slot = 3

      def connect   ()(implicit tx: S#Tx): Unit = {
        graph.changed ---> this
        // StateEvent    ---> this
      }
      def disconnect()(implicit tx: S#Tx): Unit = {
        graph.changed -/-> this
        // StateEvent    -/-> this
      }

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Update] = {
        // val graphOpt = if (graphemes .isSource(pull)) graphemes .pullUpdate(pull) else None
        val graphCh  = graph.changed
        val graphOpt = if (pull.contains(graphCh   )) pull(graphCh   ) else None
        // val stateOpt = if (pull.contains(StateEvent)) pull(StateEvent) else None

        val seq0 = graphOpt.fold(Vec.empty[Change]) { u =>
          Vec(Patch.GraphChange(u))
        }
        //        val seq1 = attrOpt.fold(seq0) { u =>
        //          if (seq0.isEmpty) u.changes else seq0 ++ u.changes
        //        }
        //        val seq3 = stateOpt.fold(seq1) { u =>
        //          if (seq1.isEmpty) u.changes else seq1 ++ u.changes
        //        }
        if (seq0.isEmpty) None else Some(Update(seq0))
      }
    }

    final def select(slot: Int): Event[S, Any, Any] = (slot: @switch) match {
      case changed    .slot => changed
      // case StateEvent .slot => StateEvent
    }

    final protected def writeData(out: DataOutput): Unit = {
      out.writeInt(SER_VERSION)
      graph.write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      graph       .dispose()
    }

    override def toString() = s"Patch$id"
  }

  // import HasAttributes.attributeEntryInfo

  private final class New[S <: Sys[S]](implicit tx0: S#Tx) extends Impl[S] {
    protected val targets       = evt.Targets[S](tx0)
    val graph                   = SynthGraphs.newVar(SynthGraphs.empty)
  }

  private final class Read[S <: Sys[S]](in: DataInput, access: S#Acc, protected val targets: evt.Targets[S])
                                       (implicit tx0: S#Tx)
    extends Impl[S] {

    {
      val serVer = in.readInt()
      if (serVer != SER_VERSION) sys.error(s"Incompatible serialized (found $serVer, required $SER_VERSION)")
    }

    val graph = SynthGraphs.readVar(in, access)
  }
}
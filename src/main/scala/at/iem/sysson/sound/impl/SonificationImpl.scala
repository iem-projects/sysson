/*
 *  SonificationImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2016 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package sound
package impl

import at.iem.sysson.sound.Sonification.Source
import de.sciss.lucre.event.{Targets, Event, Pull}
import de.sciss.lucre.expr.{DoubleObj, StringObj}
import de.sciss.lucre.matrix.Matrix
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Elem, NoSys, Obj, Sys}
import de.sciss.lucre.{event => evt, stm}
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.synth.proc.Proc

object SonificationImpl {
  private final val SER_VERSION = 0x53726300  // "Src\0"

  def sourceSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Source[S]] = anySourceSer.asInstanceOf[SourceSer[S]]

  def applySource[S <: Sys[S]](matrix: Matrix[S])(implicit tx: S#Tx): Source[S] = {
    implicit val str = StringObj.serializer[S]
    val targets = evt.Targets[S]
    val dims    = evt.Map.Modifiable[S, String, StringObj]
    new SourceImpl(targets, matrix, dims).connect()
  }

//  def mkCopy()(implicit tx: S#Tx): Sonification.Elem[S] = {
//    val Proc.Obj(proc) = Obj.copy(peer.proc)
//    val sources = expr.Map.Modifiable[S, String, Sonification.Source[S], Sonification.Source.Update[S]]
//    peer.sources.iterator.foreach { case (key, value) =>
//      sources.put(key, value.mkCopy())
//    }
//    val controls = expr.Map.Modifiable[S, String, Expr[S, Double], model.Change[Double]]
//    peer.controls.iterator.foreach {
//      case (key, DoubleObj.Var(vr))  => controls.put(key, DoubleObj.newVar(vr()))
//      case (key, value)         => controls.put(key, value)
//    }
//    val sonif = SonificationImpl.copy[S](proc, sources, controls)
//    Sonification.Elem(sonif)
//  }

  // ---- internals ----

  private val anySourceSer = new SourceSer[NoSys]

  def readIdentifiedSource[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Source[S] = {
    val targets = Targets.read(in, access)
    val cookie  = in.readInt()
    if (cookie != SER_VERSION)
      sys.error(s"Unexpected cookie (expected ${SER_VERSION.toHexString}, found ${cookie.toHexString})")

    val matrix    = Matrix.read(in, access)
    val dims      = evt.Map.read[S, String, StringObj](in, access)
    new SourceImpl(targets, matrix, dims)
  }

  private final class SourceSer[S <: Sys[S]] extends ObjSerializer[S, Source[S]] {
    def tpe: Obj.Type = Source
  }

  // XXX TODO: listen to matrix
  private final class SourceImpl[S <: Sys[S]](protected val targets: Targets[S],
                                        val matrix: Matrix[S],
                                        val dims: evt.Map[S, String, StringObj])
    extends Source[S]
    with evt.impl.SingleNode[S, Source.Update[S]] {
    source =>

//    def mkCopy()(implicit tx: S#Tx): Source[S] = {
//      val tgt       = evt.Targets[S]
//      val matrixCpy = matrix.mkCopy()
//      val dimsCpy   = expr.Map.Modifiable[S, String, Expr[S, String], model.Change[String]]
//      dims.iterator.foreach { case (key, value) =>
//        val valueCpy = value match {
//          case StringObj.Var(vr) => StringObj.newVar(vr())
//          case other => other
//        }
//        dimsCpy.put(key, valueCpy)
//      }
//      new SourceImpl[S](tgt, matrixCpy, dimsCpy)
//    }

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: stm.Copy[S, Out]): Elem[Out] = {
      val targetsOut  = Targets[Out]
      val matrixOut   = context(matrix)
      val dimsOut     = evt.Map.Modifiable[Out, String, StringObj]
      val res         = new SourceImpl(targetsOut, matrixOut, dimsOut).connect()
      context.defer(source, res) {
        dims.iterator.foreach { case (k, v) =>
          dimsOut.put(k, context(v))
        }
      }
      res
    }

    private def disconnect()(implicit tx: S#Tx): Unit = dims.changed ---> changed

    def connect   ()(implicit tx: S#Tx): this.type = {
      dims.changed -/-> changed
      this
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      disconnect()
      dims.dispose()
    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeInt(SER_VERSION)
      matrix.write(out)
      dims.write(out)
    }

    object changed extends Changed {
      def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[Source.Update[S]] =
        pull(dims.changed).map { u =>
          Source.DimsChanged(source, u)
        }
    }
  }

  def apply[S <: Sys[S]](implicit tx: S#Tx): Sonification[S] = new New[S].connect()

  /** NOTE: does not copy the arguments but assumes they have been! */
  def copy[S <: Sys[S]](proc: Proc[S], sources: evt.Map.Modifiable[S, String, Source],
                        controls: evt.Map.Modifiable[S, String, DoubleObj])
                       (implicit tx: S#Tx): Sonification[S] = {
    val targets = evt.Targets[S]
    new Copy(targets, proc, sources, controls).connect()
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Sonification[S] =
    serializer[S].read(in, access)

  def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Sonification[S]] =
    anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[NoSys]

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Sonification[S] = {
    val targets = Targets.read(in, access)
    new Read[S](in, access, targets)
  }

  private class Ser[S <: Sys[S]] extends ObjSerializer[S, Sonification[S]] {
    def tpe: Obj.Type = Sonification
  }

  private sealed trait Impl[S <: Sys[S]]
    extends Sonification[S] with evt.Node[S] /* with HasAttributes[S, Sonification[S]] */ {

    sonif =>

    type Update       = Sonification.Update[S]
    type Change       = Sonification.Change[S]

    final def event(slot: Int): Event[S, Any] = slot match {
      case changed    .slot => changed
      //      case attr .slot => attr
      // case StateEvent .slot => StateEvent
    }

    final def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: stm.Copy[S, Out]): Elem[Out] = {
      val targetsOut  = Targets[Out]
      val procOut     = context(proc)
      val sourcesOut  = evt.Map.Modifiable[Out, String, Sonification.Source]
      val controlsOut = evt.Map.Modifiable[Out, String, DoubleObj]
      val res         = new Copy[Out](targetsOut, procOut, sourcesOut, controlsOut).connect()
      context.defer(sonif, res) {
        sources.iterator.foreach { case (k, v) =>
          sourcesOut.put(k, context[Sonification.Source](v))
        }
        controls.iterator.foreach { case (k, v) =>
          controlsOut.put(k, context[DoubleObj](v))
        }
      }
      res
    }

    // final protected def reader: evt.Reader[S, Sonification[S]] = SonificationImpl.serializer

    //    final protected def AssociationAdded  (key: String) = Sonification.AttributeAdded  [S](key)
    //    final protected def AssociationRemoved(key: String) = Sonification.AttributeRemoved[S](key)
    //    final protected def AttributeChange   (key: String, u: Elem.Update[S]) =
    //      Sonification.AttributeChange(key, u.element, u.change)

    final protected def Update(changes: Vec[Change]) = Sonification.Update(sonif, changes)

    /* sealed */ protected trait SelfEvent {
      // final protected def reader: evt.Reader[S, Sonification[S]] = sonif.reader
      final def node: Sonification[S] with evt.Node[S] = sonif
    }

    final def connect   ()(implicit tx: S#Tx): this.type = {
      proc.changed ---> changed
      // attr    ---> this
      // StateEvent    ---> this
      this
    }

    private def disconnect()(implicit tx: S#Tx): Unit = {
      proc.changed -/-> changed
      // attr    -/-> this
      // StateEvent    -/-> this
    }

    object changed
//      extends evt.impl.EventImpl[S, Update, Sonification[S]]
//      with evt.InvariantEvent   [S, Update, Sonification[S]]
      extends evt.Event[S, Update]
      with SelfEvent {

      final val slot = 3

      // XXX TODO: for completeness, should forward changes to sources and controls!
      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Update] = {
        val procCh    = proc.changed
        val procOpt   = if (pull.contains(procCh   )) pull(procCh   ) else None
        // val attrOpt  = if (pull.contains(attr)) pull(attr) else None
        // val stateOpt = if (pull.contains(StateEvent)) pull(StateEvent) else None

        val seq0 = procOpt.fold(Vec.empty[Change]) { u =>
          u.changes.map(Sonification.ProcChange.apply)
        }
        //        val seq1 = attrOpt.fold(seq0) { u =>
        //          if (seq0.isEmpty) u.changes else seq0 ++ u.changes
        //        }
        //        val seq3 = stateOpt.fold(seq0) { u =>
        //          if (seq0.isEmpty) u.changes else seq0 ++ u.changes
        //        }
        if (seq0.isEmpty) None else Some(Update(seq0))
      }
    }

//    final def select(slot: Int): Event[S, Any] = slot match {
//      case changed    .slot => changed
//      //      case attr .slot => attr
//      // case StateEvent .slot => StateEvent
//    }

    final protected def writeData(out: DataOutput): Unit = {
      out.writeInt(SER_VERSION)
      proc        .write(out)
      sources     .write(out)
      controls    .write(out)
      //      attributeMap.write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      disconnect()
      proc        .dispose()
      sources     .dispose()
      controls    .dispose()
      //      attributeMap.dispose()
    }

    override def toString() = "Sonification" + id
  }

  // import HasAttributes.attributeEntryInfo

  private final class Copy[S <: Sys[S]](protected val targets: evt.Targets[S],
                                        val proc: Proc[S],
                                        val sources: evt.Map.Modifiable[S, String, Source],
                                        val controls: evt.Map.Modifiable[S, String, DoubleObj])
    extends Impl[S]

  private final class New[S <: Sys[S]](implicit tx0: S#Tx) extends Impl[S] {
    protected val targets       = evt.Targets[S](tx0)
    // val patch                   = Obj(Patch.Elem(Patch[S]))
    val proc                    = Proc[S]
    val sources                 = evt.Map.Modifiable[S, String, Source]
    val controls                = {
      // implicit val ser = DoubleObj.serializer[S]
      evt.Map.Modifiable[S, String, DoubleObj]
    }
  }

  private final class Read[S <: Sys[S]](in: DataInput, access: S#Acc, protected val targets: evt.Targets[S])
                                       (implicit tx0: S#Tx)
    extends Impl[S] {

    {
      val serVer = in.readInt()
      if (serVer != SER_VERSION) sys.error(s"Incompatible serialized (found $serVer, required $SER_VERSION)")
    }

    val proc        = Proc.read(in, access)
    val sources     = evt.Map.read[S, String, Source](in, access)
    val controls    = {
      implicit val ser = DoubleObj.serializer[S]
      evt.Map.read[S, String, DoubleObj](in, access)
    }

    //    protected val attributeMap  = SkipList.Map.read[S, String, AttributeEntry](in, access)
  }
}

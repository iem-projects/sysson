/*
 *  SonificationImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.lucre.event.{Event, EventLike, InMemory, Pull, Sys}
import de.sciss.lucre.expr.{Double => DoubleEx, Expr, String => StringEx}
import de.sciss.lucre.matrix.Matrix
import de.sciss.lucre.{event => evt, expr}
import de.sciss.model
import de.sciss.serial.{DataInput, DataOutput}
import de.sciss.synth.proc.impl.{ActiveElemImpl, ElemCompanionImpl}
import de.sciss.synth.proc.{Elem, Obj, Proc}

object SonificationImpl {
  private final val SER_VERSION = 0x53726300  // "Src\0"

  def sourceSerializer[S <: Sys[S]]: evt.NodeSerializer[S, Source[S]] = anySourceSer.asInstanceOf[SourceSer[S]]

  def applySource[S <: Sys[S]](matrix: Matrix[S])(implicit tx: S#Tx): Source[S] = {
    implicit val str = StringEx.serializer[S]
    val targets = evt.Targets[S]
    val dims    = expr.Map.Modifiable[S, String, Expr[S, String], model.Change[String]]
    new SourceImpl(targets, matrix, dims)
  }

  // ---- elem ----

  object SonificationElemImpl extends ElemCompanionImpl[Sonification.Elem] {
    final val typeID = Sonification.typeID

    private lazy val _init: Unit = Elem.registerExtension(this)

    def init(): Unit = _init

    def apply[S <: Sys[S]](peer: Sonification[S])(implicit tx: S#Tx): Sonification.Elem[S] = {
      val targets = evt.Targets[S]
      new Impl[S](targets, peer)
    }

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Sonification.Elem[S] =
      serializer[S].read(in, access)

    // ---- Elem.Extension ----

    /** Read identified active element */
    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                   (implicit tx: S#Tx): Sonification.Elem[S] with evt.Node[S] = {
      val peer = Sonification.read(in, access)
      new Impl[S](targets, peer)
    }

    /** Read identified constant element */
    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Sonification.Elem[S] =
      sys.error("Constant Sonification not supported")

    // ---- implementation ----

    private final class Impl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                          val peer: Sonification[S])
      extends Sonification.Elem[S]
      with ActiveElemImpl[S] {

      def typeID = SonificationElemImpl.typeID
      def prefix = "Sonification"

      override def toString() = s"$prefix$id"

      def mkCopy()(implicit tx: S#Tx): Sonification.Elem[S] = {
        val Proc.Obj(proc) = Obj.copy(peer.proc)
        val sources = expr.Map.Modifiable[S, String, Sonification.Source[S], Sonification.Source.Update[S]]
        peer.sources.iterator.foreach { case (key, value) =>
          sources.put(key, value.mkCopy())
        }
        import DoubleEx.{serializer => doubleSer, varSerializer => doubleVarSer}
        val controls = expr.Map.Modifiable[S, String, Expr[S, Double], model.Change[Double]]
        peer.controls.iterator.foreach {
          case (key, Expr.Var(vr))  => controls.put(key, DoubleEx.newVar(vr()))
          case (key, value)         => controls.put(key, value)
        }
        val sonif = SonificationImpl.copy[S](proc, sources, controls)
        Sonification.Elem(sonif)
      }
    }
  }

  // ---- internals ----

  private val anySourceSer = new SourceSer[evt.InMemory]

  private final class SourceSer[S <: Sys[S]] extends evt.NodeSerializer[S, Source[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Source[S] = {
      implicit val str = StringEx.serializer[S]
      val cookie = in.readInt()
      require(cookie == SER_VERSION,
        s"Unexpected cookie (expected ${SER_VERSION.toHexString}, found ${cookie.toHexString})")
      val matrix    = Matrix.read(in, access)
      val dims      = expr.Map.read[S, String, Expr[S, String], model.Change[String]](in, access)
      new SourceImpl(targets, matrix, dims)
    }
  }

  // XXX TODO: listen to matrix
  private final class SourceImpl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                        val matrix: Matrix[S],
                                        val dims: expr.Map[S, String, Expr[S, String], model.Change[String]])
    extends Source[S]
    with evt.impl.StandaloneLike[S, Source.Update[S], Source[S]] {
    source =>

    def mkCopy()(implicit tx: S#Tx): Source[S] = {
      val tgt       = evt.Targets[S]
      val matrixCpy = matrix.mkCopy()
      import StringEx.{varSerializer => stringVarSer, serializer => stringSer}
      val dimsCpy   = expr.Map.Modifiable[S, String, Expr[S, String], model.Change[String]]
      dims.iterator.foreach { case (key, value) =>
        val valueCpy = value match {
          case Expr.Var(vr) => StringEx.newVar(vr())
          case other => other
        }
        dimsCpy.put(key, valueCpy)
      }
      new SourceImpl[S](tgt, matrixCpy, dimsCpy)
    }

    def disconnect()(implicit tx: S#Tx): Unit = dims.changed ---> this
    def connect   ()(implicit tx: S#Tx): Unit = dims.changed -/-> this

    protected def disposeData()(implicit tx: S#Tx): Unit = dims.dispose()

    protected def writeData(out: DataOutput): Unit = {
      out.writeInt(SER_VERSION)
      matrix.write(out)
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

  /** NOTE: does not copy the arguments but assumes they have been! */
  def copy[S <: Sys[S]](proc: Proc.Obj[S], sources: expr.Map.Modifiable[S, String, Source[S], Source.Update[S]],
                        controls: expr.Map.Modifiable[S, String, Expr[S, Double], model.Change[Double]])
                       (implicit tx: S#Tx): Sonification[S] = {
    val targets = evt.Targets[S]
    new Copy(targets, proc, sources, controls)
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Sonification[S] =
    serializer[S].read(in, access)

  def serializer[S <: Sys[S]]: evt.NodeSerializer[S, Sonification[S]] =
    anySer.asInstanceOf[evt.NodeSerializer[S, Sonification[S]]]

  private val anySer = new Serializer[InMemory]

  private class Serializer[S <: Sys[S]] extends evt.NodeSerializer[S, Sonification[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Sonification[S] =
      new Read[S](in, access, targets)
  }

  private sealed trait Impl[S <: Sys[S]] extends Sonification[S] /* with HasAttributes[S, Sonification[S]] */ {
    sonif =>

    type Update       = Sonification.Update[S]
    type Change       = Sonification.Change[S]

    final protected def reader: evt.Reader[S, Sonification[S]] = SonificationImpl.serializer

    //    final protected def AssociationAdded  (key: String) = Sonification.AttributeAdded  [S](key)
    //    final protected def AssociationRemoved(key: String) = Sonification.AttributeRemoved[S](key)
    //    final protected def AttributeChange   (key: String, u: Elem.Update[S]) =
    //      Sonification.AttributeChange(key, u.element, u.change)

    final protected def Update(changes: Vec[Change]) = Sonification.Update(sonif, changes)

    /* sealed */ protected trait SelfEvent {
      final protected def reader: evt.Reader[S, Sonification[S]] = sonif.reader
      final def node: Sonification[S] = sonif
    }

    object changed
      extends evt.impl.EventImpl[S, Update, Sonification[S]]
      with evt.InvariantEvent   [S, Update, Sonification[S]]
      with SelfEvent {

      final val slot = 3

      def connect   ()(implicit tx: S#Tx): Unit = {
        proc.changed ---> this
        // attr    ---> this
        // StateEvent    ---> this
      }
      def disconnect()(implicit tx: S#Tx): Unit = {
        proc.changed -/-> this
        // attr    -/-> this
        // StateEvent    -/-> this
      }

      // XXX TODO: for completeness, should forward changes to sources and controls!
      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Update] = {
        val procCh    = proc.elem.peer.changed
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

    final def select(slot: Int): Event[S, Any, Any] = slot match {
      case changed    .slot => changed
      //      case attr .slot => attr
      // case StateEvent .slot => StateEvent
    }

    final protected def writeData(out: DataOutput): Unit = {
      out.writeInt(SER_VERSION)
      proc        .write(out)
      sources     .write(out)
      controls    .write(out)
      //      attributeMap.write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      proc        .dispose()
      sources     .dispose()
      controls    .dispose()
      //      attributeMap.dispose()
    }

    override def toString() = "Sonification" + id
  }

  // import HasAttributes.attributeEntryInfo

  private final class Copy[S <: Sys[S]](protected val targets: evt.Targets[S],
                                        val proc: Proc.Obj[S],
                                        val sources: expr.Map.Modifiable[S, String, Source[S], Source.Update[S]],
                                        val controls: expr.Map.Modifiable[S, String, Expr[S, Double], model.Change[Double]])
    extends Impl[S]

  private final class New[S <: Sys[S]](implicit tx0: S#Tx) extends Impl[S] {
    protected val targets       = evt.Targets[S](tx0)
    // val patch                   = Obj(Patch.Elem(Patch[S]))
    val proc                    = Obj(Proc.Elem(Proc[S]))
    val sources                 = expr.Map.Modifiable[S, String, Source[S], Source.Update[S]]
    val controls                = {
      implicit val ser = DoubleEx.serializer[S]
      expr.Map.Modifiable[S, String, Expr[S, Double], model.Change[Double]]
    }
    //    protected val attributeMap  = SkipList.Map.empty[S, String, AttributeEntry]
  }

  private final class Read[S <: Sys[S]](in: DataInput, access: S#Acc, protected val targets: evt.Targets[S])
                                       (implicit tx0: S#Tx)
    extends Impl[S] {

    {
      val serVer = in.readInt()
      require(serVer == SER_VERSION, s"Incompatible serialized (found $serVer, required $SER_VERSION)")
    }

    // val patch                   = Obj.readT[S, Patch.Elem](in, access)
    val proc        = Obj.readT[S, Proc.Elem](in, access)
    val sources     = expr.Map.read[S, String, Source[S], Source.Update[S]](in, access)
    val controls    = {
      implicit val ser = DoubleEx.serializer[S]
      expr.Map.read[S, String, Expr[S, Double], model.Change[Double]](in, access)
    }

    //    protected val attributeMap  = SkipList.Map.read[S, String, AttributeEntry](in, access)
  }
}

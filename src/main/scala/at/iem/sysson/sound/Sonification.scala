/*
 *  Sonification.scala
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

import at.iem.sysson.sound.impl.{SonificationImpl => Impl}
import de.sciss.lucre.event.Publisher
import de.sciss.lucre.expr.{DoubleObj, StringObj}
import de.sciss.lucre.matrix.Matrix
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.synth.proc.Proc

object Sonification extends Obj.Type {
  final val typeID = 0x30004

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)

  // ---- implementation forwards ----

  def apply[S <: Sys[S]](implicit tx: S#Tx): Sonification[S] = Impl[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Sonification[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Sonification[S]] = Impl.serializer[S]

  // ---- event types ----

  /** An update is a sequence of changes */
  final case class Update[S <: Sys[S]](sonif: Sonification[S], changes: Vec[Change[S]])

  /** A change is either a state change, or a scan or a grapheme change */
  sealed trait Change[S <: Sys[S]]

  //  /** A state change is either a renaming, a change of graph, or a change of association (map) */
  //  sealed trait StateChange[S <: Sys[S]] extends Change[S]

  final case class ProcChange[S <: Sys[S]](change: Proc.Change[S]) extends Change[S]

  // -------------------------------------------------------

  object Source extends Obj.Type {
    final val typeID = 0x30007

    def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
      Impl.readIdentifiedSource(in, access)

    sealed trait Update[S <: Sys[S]] { def source: Source[S] }
    final case class DimsChanged[S <: Sys[S]](source: Source[S],
                                              update: evt.Map.Update[S, String, StringObj])
      extends Update[S]

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Source[S]] = Impl.sourceSerializer

    def apply[S <: Sys[S]](matrix: Matrix[S])(implicit tx: S#Tx): Source[S] =
      Impl.applySource(matrix)
  }
  /** A sonification source is a matrix paired with a dimensional map.
    *
    * @see [[graph.Var]]
    * @see [[graph.Dim]]
    */
  trait Source[S <: Sys[S]] extends Obj[S] with Publisher[S, Source.Update[S]] {
    final def tpe: Obj.Type = Source

    def matrix: Matrix[S]
    /** Maps sonification model/patch dimensions (keys) to source matrix dimensions (values). */
    def dims: evt.Map[S, String, StringObj]

    // def mkCopy()(implicit tx: S#Tx): Source[S]
  }

  // ---- element ----

//  object Elem {
//    def apply[S <: Sys[S]](peer: Sonification[S])(implicit tx: S#Tx): Sonification.Elem[S] =
//      Impl.SonificationElemImpl(peer)
//
//    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Sonification.Elem[S]] =
//      Impl.SonificationElemImpl.serializer
//  }
//  trait Elem[S <: Sys[S]] extends proc.Elem[S] {
//    type Peer       = Sonification[S]
//    type PeerUpdate = Sonification.Update[S]
//    type This       = Elem[S]
//  }
//
//  object Obj {
//    def unapply[S <: Sys[S]](obj: Obj[S]): Option[Sonification.Obj[S]] =
//      if (obj.elem.isInstanceOf[Sonification.Elem[S]]) Some(obj.asInstanceOf[Sonification.Obj[S]])
//      else None
//  }
//
//  type Obj[S <: Sys[S]] = proc.Obj.T[S, Sonification.Elem]
}
/** A sonification pairs a sound process with a map to data sources and user controls. */
trait Sonification[S <: Sys[S]] extends Obj[S] with Publisher[S, Sonification.Update[S]] {
  /** The sound process that implements the sonification */
  def proc: Proc[S]

  final def tpe: Obj.Type = Sonification

  /** A map from logical keys to sonification sources. A source is
    * a matrix paired with a dimensional map.
    *
    * @see [[graph.Var]]
    * @see [[graph.Dim]]
    */
  def sources : evt.Map[S, String, Sonification.Source]

  /** A map from logical keys to control values.
    *
    * @see [[graph.UserValue]]
    */
  def controls: evt.Map[S, String, DoubleObj]
}
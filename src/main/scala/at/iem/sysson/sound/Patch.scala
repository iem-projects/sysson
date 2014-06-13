///*
// *  Patch.scala
// *  (SysSon)
// *
// *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
// *  Written by Hanns Holger Rutz.
// *
// *	This software is published under the GNU General Public License v3+
// *
// *
// *	For further information, please contact Hanns Holger Rutz at
// *	contact@sciss.de
// */
//
//package at.iem.sysson
//package sound
//
//import de.sciss.lucre.{event => evt}
//import evt.{Publisher, Sys}
//import de.sciss.lucre.expr.Expr
//import de.sciss.synth.{proc, SynthGraph}
//import de.sciss.synth.proc.{Obj, Elem, AttrMap}
//import impl.{PatchImpl => Impl}
//import de.sciss.serial.{Serializer, DataInput}
//import de.sciss.model
//
//object Patch {
//  final val typeID = 0x30000
//
//  // ---- implementation forwards ----
//
//  def apply[S <: Sys[S]](implicit tx: S#Tx): Patch[S] = Impl[S]
//
//  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Patch[S] = Impl.read(in, access)
//
//  implicit def serializer[S <: Sys[S]]: evt.NodeSerializer[S, Patch[S]] = Impl.serializer[S]
//
//  // ---- event types ----
//
//  /** An update is a sequence of changes */
//  final case class Update[S <: Sys[S]](patch: Patch[S], changes: Vec[Change[S]])
//
//  /** A change is either a state change, or a scan or a grapheme change */
//  sealed trait Change[S <: Sys[S]]
//
//  //  /** A state change is either a renaming, a change of graph, or a change of association (map) */
//  //  sealed trait StateChange[S <: Sys[S]] extends Change[S]
//
//  final case class GraphChange[S <: Sys[S]](change: model.Change[SynthGraph]) extends Change[S]
//
//  // ---- elem ----
//
//  object Elem {
//    def apply[S <: Sys[S]](peer: Patch[S])(implicit tx: S#Tx): Patch.Elem[S] = Impl.PatchElemImpl(peer)
//
//    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Patch.Elem[S]] =
//      Impl.PatchElemImpl.serializer
//
//    object Obj {
//      def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, Patch.Elem]] =
//        if (obj.elem.isInstanceOf[Patch.Elem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, Patch.Elem]])
//        else None
//    }
//  }
//  trait Elem[S <: Sys[S]] extends proc.Elem[S] {
//    type Peer       = Patch[S]
//    type PeerUpdate = Patch.Update[S]
//
//    def mkCopy()(implicit tx: S#Tx): Elem[S]
//  }
//}
//trait Patch[S <: Sys[S]] extends evt.Node[S] with Publisher[S, Patch.Update[S]] {
//  def graph: Expr.Var[S, SynthGraph]
//}
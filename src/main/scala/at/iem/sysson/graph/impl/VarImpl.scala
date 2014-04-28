///*
// *  VarImpl.scala
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
//package graph
//package impl
//
//import at.iem.sysson.graph.Var
//import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
//
//object VarImpl {
//  // def Default: Var = Impl(Vec.empty)
//
//  private final val VAR_COOKIE = 0x76617200 // "var\0"
//
//  // def apply(name: String, dims: Vec[Dim], higherRank: Boolean): Var = Impl(name)(dims, higherRank, Vec.empty)
//
//  implicit object serializer extends ImmutableSerializer[Var] {
//    def read(in: DataInput): Var = {
//      val cookie = in.readInt()
//      require(cookie == VAR_COOKIE,
//        s"Unexpected cookie (expected ${VAR_COOKIE.toHexString}, found ${cookie.toHexString})")
//      val name        = in.readUTF()
//      // val dims        = ImmutableSerializer.indexedSeq[Dim].read(in)
//      val higherRank  = in.readBoolean()
//      Var(name, /* dims, */ higherRank)
//    }
//
//    def write(v: Var, out: DataOutput): Unit = {
//      out.writeInt(VAR_COOKIE)
//      out.writeUTF(v.name)
//      // ImmutableSerializer.indexedSeq[Dim].write(v.dims, out)
//      out.writeBoolean(v.higherRank)
//    }
//  }
//}

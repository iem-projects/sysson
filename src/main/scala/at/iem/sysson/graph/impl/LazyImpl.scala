///*
// *  LazyImpl.scala
// *  (SysSon)
// *
// *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
// *  Written by Hanns Holger Rutz.
// *
// *	This software is published under the GNU General Public License v2+
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
//import de.sciss.synth.{UGenGraph, UGenInLike, GE}
//import at.iem.sysson.sound.UGenGraphBuilderOLD
//
//trait LazyImpl extends GE.Lazy {
//  protected final def makeUGens: UGenInLike =
//    UGenGraph.builder match {
//      case b: UGenGraphBuilderOLD => makeUGens(b)
//      case _ => sys.error(s"Expansion out of context: $this")
//    }
//
//  protected def makeUGens(builder: UGenGraphBuilderOLD): UGenInLike
//}

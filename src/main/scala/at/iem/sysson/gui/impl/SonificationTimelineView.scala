///*
// *  SonificationTimelineView.scala
// *  (SysSon)
// *
// *  Copyright (c) 2013-2015 Institute of Electronic Music and Acoustics, Graz.
// *  Copyright (c) 2014-2015 Hanns Holger Rutz. All rights reserved.
// *
// *	This software is published under the GNU General Public License v3+
// *
// *
// *	For further information, please contact Hanns Holger Rutz at
// *	contact@sciss.de
// */
//
//package at.iem.sysson
//package gui
//package impl
//
//import at.iem.sysson.sound.Sonification
//import de.sciss.lucre.bitemp.{SpanLike => SpanLikeEx}
//import de.sciss.lucre.event.Sys
//import de.sciss.lucre.expr.Expr
//import de.sciss.lucre.stm
//import de.sciss.mellite.gui.TimelineObjView
//import de.sciss.mellite.gui.TimelineObjView.Context
//import de.sciss.mellite.gui.impl.timeline.TimelineObjViewImpl
//import de.sciss.span.SpanLike
//import de.sciss.synth.proc.FadeSpec
//
//object SonificationTimelineView extends TimelineObjView.Factory {
//  def typeID: Int = Sonification.typeID
//
//  type E[S <: Sys[S]] = Sonification.Elem[S]
//
//  private lazy val _init: Unit = TimelineObjView.addFactory(this)
//
//  def init(): Unit = _init
//
//
//}
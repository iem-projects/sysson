///*
// *  GUI.scala
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
//package gui
//
//import scala.swing.{Component, Swing}
//import at.iem.sysson.gui.impl.DataSourceObjView
//
//object GUI {
//  def fixSize(c: Component): Unit = {
//    val d = c.preferredSize
//    c.preferredSize = d
//    c.minimumSize   = d
//    c.maximumSize   = d
//  }
//  private def wordWrap(s: String, margin: Int = 80): String = {
//    if (s == null) return "" // fuck java
//    val sz = s.length
//    if (sz <= margin) return s
//    var i = 0
//    val sb = new StringBuilder
//    while (i < sz) {
//      val j = s.lastIndexOf(" ", i + margin)
//      val found = j > i
//      val k = if (found) j else i + margin
//      sb.append(s.substring(i, math.min(sz, k)))
//      i = if (found) k + 1 else k
//      if (i < sz) sb.append('\n')
//    }
//    sb.toString()
//  }
//
//  def formatException(e: Throwable): String = {
//    e.getClass.toString + " :\n" + wordWrap(e.getMessage) + "\n" +
//      e.getStackTrace.take(10).map("   at " + _).mkString("\n")
//  }
//
//  //  def windowOption(component: Component): Option[desktop.Window] =
//  //    Option(component.peer.getClientProperty(impl.WindowImpl.WindowKey).asInstanceOf[desktop.Window])
//
//  def registerViews(): Unit = {
//    DataSourceObjView
//  }
//}
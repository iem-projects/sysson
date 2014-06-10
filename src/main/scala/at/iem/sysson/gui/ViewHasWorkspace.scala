///*
// *  ViewHasWorkspace.scala
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
//package at.iem.sysson.gui
//
//import de.sciss.lucre.event.Sys
//import de.sciss.lucre.stm
//import de.sciss.lucre.swing.{View => SView}
//import de.sciss.mellite.Workspace
//
//trait ViewHasWorkspace[S <: Sys[S]] extends SView.Cursor[S] {
//  def workspace: Workspace[S]
//  implicit def cursor: stm.Cursor[S] // = workspace.cursor
//}

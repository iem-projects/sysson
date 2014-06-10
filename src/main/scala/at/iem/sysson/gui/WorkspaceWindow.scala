///*
// *  WorkspaceWindow.scala
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
//import de.sciss.lucre.event.Sys
//import impl.{WorkspaceWindowImpl => Impl}
//import de.sciss.lucre.swing.Window
//import de.sciss.mellite.Workspace
//
//object WorkspaceWindow {
//  /** Opens a workspace window for a given workspace. When the window is closed, the workspace is disposed. */
//  private[sysson] def apply[S <: Sys[S]](workspace: Workspace[S])(implicit tx: S#Tx): WorkspaceWindow[S] =
//    Impl(workspace)
//}
//trait WorkspaceWindow[S <: Sys[S]] extends Window[S] {
//  def view: WorkspaceView[S]
//}
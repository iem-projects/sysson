///*
// *  WorkspaceView.scala
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
//import impl.{WorkspaceViewImpl => Impl}
//import de.sciss.lucre.event.Sys
//import de.sciss.lucre.swing.View
//import de.sciss.mellite.Workspace
//
//object WorkspaceView {
//  def apply[S <: Sys[S]](workspace: Workspace[S])(implicit tx: S#Tx): WorkspaceView[S] = Impl(workspace)
//}
//trait WorkspaceView[S <: Sys[S]] extends ViewHasWorkspace[S] with View.Editable[S] with View.File
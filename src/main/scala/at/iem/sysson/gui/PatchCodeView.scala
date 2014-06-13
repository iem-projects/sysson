/*
 *  PatchCodeView.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import de.sciss.lucre.event.Sys
import de.sciss.desktop.UndoManager
import de.sciss.lucre.stm
import de.sciss.mellite.Workspace
import de.sciss.mellite.gui.ViewHasWorkspace
import impl.{PatchCodeViewImpl => Impl}
import de.sciss.model.Model
import scala.swing.Action
import de.sciss.lucre.expr.Expr
import de.sciss.synth.SynthGraph
import scala.concurrent.Future

object PatchCodeView {
  def apply[S <: Sys[S]](entry: Library.Leaf[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S],
                         undoManager: UndoManager): PatchCodeView[S] =
    Impl(entry.source, graph = None)

  /** If `graph` is given, the `apply` action is tied to updating the graph variable. */
  def apply[S <: Sys[S]](sourceCode: Expr.Var[S, String], graph: Option[Expr.Var[S, SynthGraph]])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S],
                         undoManager: UndoManager): PatchCodeView[S] =
    Impl(sourceCode, graph)

  sealed trait Update
  case class DirtyChange(value: Boolean) extends Update
}
trait PatchCodeView[S <: Sys[S]] extends ViewHasWorkspace[S] with Model[PatchCodeView.Update] {
  def isCompiling: Boolean

  def dirty: Boolean

  def save(): Future[Unit]

  def undoAction: Action
  def redoAction: Action
}
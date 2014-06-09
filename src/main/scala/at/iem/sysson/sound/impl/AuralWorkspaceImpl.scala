/*
 *  AuralWorkspaceImpl.scala
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
package sound
package impl

import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm.{IdentifierMap, Disposable}
import scala.concurrent.Future
import de.sciss.synth.proc.{Obj, Grapheme}
import de.sciss.lucre.synth

object AuralWorkspaceImpl {
  def apply[S <: Sys[S], I1 <: synth.Sys[I1]](workspace: Workspace[S] { type I = I1 })
                                       (implicit tx: S#Tx): AuralWorkspace[S, I1] = {
    val map = tx.newInMemoryIDMap[AuralSonification[S]]
    val res = new Impl(workspace, map)
    // de.sciss.lucre.synth.showLog = true
    workspace.addDependent(res)
    res
  }

  private final class Impl[S <: Sys[S], I1 <: synth.Sys[I1]](val workspace: Workspace[S] { type I = I1 },
                                        map: IdentifierMap[S#ID, S#Tx, AuralSonification[S]])
    extends AuralWorkspace[S, I1] with Disposable[S#Tx] {
    impl =>

    def view(sonif: Obj.T[S, Sonification.Elem])(implicit tx: S#Tx): AuralSonification[S] =
      map.get(sonif.id).getOrElse {
        val view = AuralSonificationImpl(impl, sonif)
        map.put(sonif.id, view)
        view
      }

    def dispose()(implicit tx: S#Tx): Unit = {
      map.dispose() // XXX TODO: iterate and dispose AuralSonification instances?
    }

    def graphemeCache(section: VariableSection)(implicit tx: S#Tx): (Grapheme.Expr.Audio[I1], Future[Unit]) = {

      ???
    }
  }
}
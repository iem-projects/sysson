/*
 *  SonificationViewImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui
package impl

import de.sciss.lucre.event.Sys
import at.iem.sysson.sound.{Keys, Sonification}
import de.sciss.lucre.stm
import scala.swing.{Swing, Orientation, BoxPanel, Label, FlowPanel, Component}
import de.sciss.synth.proc.Attribute
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.UndoManager
import de.sciss.swingplus.Separator
import de.sciss.audiowidgets.Transport

object SonificationViewImpl {
  def apply[S <: Sys[S]](workspace: Workspace[S], sonification: Sonification[S])
                        (implicit tx: S#Tx): SonificationView[S] = {
    implicit val undoMgr = new UndoManagerImpl {
      protected var dirty: Boolean = false
    }
    val sonifH    = tx.newHandle(sonification)
    val nameView  = sonification.attributes[Attribute.String](Keys.attrName).map { expr =>
      import workspace.cursor
      StringExprEditor(expr, "Name")
    }

    // val p       = sonification.patch
    // val graph0  = p.graph.value

    val res = new Impl(workspace, sonifH, nameView, undoMgr)
    // workspace.addDependent(res)
    GUI.fromTx {
      res.guiInit()
    }
    res
  }

  private final class Impl[S <: Sys[S]](val workspace: Workspace[S], sonifH: stm.Source[S#Tx, Sonification[S]],
                                        nameView: Option[StringExprEditor[S]], val undoManager: UndoManager)
    extends SonificationView[S] with ComponentHolder[Component] {

    def sonification(implicit tx: S#Tx): Sonification[S] = sonifH()

    def guiInit(): Unit = {
      // ---- Header ----
      val cHead   = nameView.fold(List.empty[Component])(view => List(new Label("Name:"), view.component))
      val pHeader = new FlowPanel(cHead: _*)

      // ---- Mapping ----

      val pMapping = new FlowPanel()
      pMapping.border = Swing.TitledBorder(Swing.EmptyBorder(4), "Mapping")

      // ---- Controls ----

      val pControls = new FlowPanel()
      pControls.border = Swing.TitledBorder(Swing.EmptyBorder(4), "Controls")

      // ---- Transport ----

      val transpButs = Transport.makeButtonStrip {
        import Transport._
        Seq(GoToBegin(), Stop(), Play(), Loop())
      }
      val pTransport = new FlowPanel(transpButs)
      pTransport.border = Swing.TitledBorder(Swing.EmptyBorder(4), "Transport")

      component = new BoxPanel(Orientation.Vertical) {
        contents += pHeader
        contents += Separator()
        contents += pMapping
        contents += Separator()
        contents += pControls
        contents += Separator()
        contents += pTransport
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      // workspace.removeDependent(this)
      nameView.foreach(_.dispose())
    }
  }
}

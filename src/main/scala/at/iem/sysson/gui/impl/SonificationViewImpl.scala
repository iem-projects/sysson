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
import scala.swing.{TextField, Alignment, Swing, Orientation, BoxPanel, Label, FlowPanel, Component}
import de.sciss.synth.proc.Attribute
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.UndoManager
import de.sciss.swingplus.Separator
import de.sciss.audiowidgets.Transport
import de.sciss.synth.SynthGraph
import scalaswingcontrib.group.GroupPanel
import javax.swing.GroupLayout
import language.reflectiveCalls

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

    val res = new Impl(workspace, sonifH, nameView)
    // workspace.addDependent(res)
    GUI.fromTx {
      res.guiInit()
    }

    val p       = sonification.patch
    val graph0  = p.graph.value
    res.updateGraph(graph0)

    res
  }

  private final class Impl[S <: Sys[S]](val workspace: Workspace[S], sonifH: stm.Source[S#Tx, Sonification[S]],
                                        nameView: Option[StringExprEditor[S]])(implicit val undoManager: UndoManager)
    extends SonificationView[S] with ComponentHolder[Component] {

    import workspace.cursor

    def sonification(implicit tx: S#Tx): Sonification[S] = sonifH()

    private var pMapping : FlowPanel = _
    private var pControls: FlowPanel = _

    // XXX TODO: private val graphDisposables = STMRef[Vec[Disposable[S#Tx]]]

    def updateGraph(g: SynthGraph)(implicit tx: S#Tx): Unit = {
      val sonif = sonifH()

      // XXX TODO: graphDisposables.swap(Vec.empty).foreach(_.dispose())

      // ---- sources/mapping tx ----

      val mapping = sonif.sources
      val sources = g.sources.collect {
        case vr: graph.Var =>
          val key   = vr.name
          // vr.name
          // vr.dims
          // vr.higherRank
          // vr.operations

          val view = SonificationSourceView(workspace, mapping, key)

          (key, view)
      }
      
      // ---- controls tx ----

      val controls    = sonif.controls
      val userValues  = g.sources.collect {
        case graph.UserValue(key, default) =>
          val view = DoubleExprEditor(controls, key, default, key.capitalize)
          (key, view)
      }

      // XXX TODO: graphDisposables.transform(_ ++ userValues.map(_._2))

      GUI.fromTx {
        // ---- sources/mapping gui ----

        val ggMap = sources.map { case (key, view) =>
          val lb    = new Label(s"$key:")
          val drop  = view.component
          (lb, drop)
        }

        val pMap = new GroupPanel {
          theHorizontalLayout is Sequential(
            Parallel(ggMap.map(tup => add[GroupLayout#ParallelGroup](tup._1)): _*),
            Parallel(ggMap.map(tup => add[GroupLayout#ParallelGroup](tup._2)): _*)
          )

          theVerticalLayout is Sequential(
            ggMap.map { tup =>
              Parallel(Baseline)(tup._1, tup._2): InGroup[GroupLayout#SequentialGroup]
            }: _*
          )
        }
        pMapping.contents.clear()
        pMapping.contents += pMap

        // ---- controls gui ----

        val ggCtl = userValues.map { case (key, /* value, */ view) =>
          val lb    = new Label(s"$key:", null, Alignment.Trailing)
          val sp    = view.component
          (lb, sp)
        }

        val pCtl = new GroupPanel {
          theHorizontalLayout is Sequential(
            Parallel(ggCtl.map(tup => add[GroupLayout#ParallelGroup](tup._1)): _*),
            Parallel(ggCtl.map(tup => add[GroupLayout#ParallelGroup](tup._2)): _*)
          )

          theVerticalLayout is Sequential(
            ggCtl.map { tup =>
              Parallel(Baseline)(tup._1, tup._2): InGroup[GroupLayout#SequentialGroup]
            }: _*
          )
        }
        pControls.contents.clear()
        pControls.contents += pCtl

        // ----

        component.revalidate()
      }
    }

    def guiInit(): Unit = {
      // ---- Header ----
      val cHead   = nameView.fold(List.empty[Component])(view => List(new Label("Name:"), view.component))
      val pHeader = new FlowPanel(cHead: _*)

      // ---- Mapping ----

      pMapping    = new FlowPanel()
      pMapping.border = Swing.TitledBorder(Swing.EmptyBorder(4), "Mapping")

      // ---- Controls ----

      pControls = new FlowPanel()
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

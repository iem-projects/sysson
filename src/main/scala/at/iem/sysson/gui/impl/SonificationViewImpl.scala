/*
 *  SonificationViewImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui
package impl

import de.sciss.lucre.event.Sys
import at.iem.sysson.sound.{AuralSonification, AuralWorkspaceHandler, Keys, Sonification}
import de.sciss.lucre.stm
import scala.swing._
import de.sciss.synth.proc.{Obj, StringElem, Elem}
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.UndoManager
import de.sciss.swingplus.{GroupPanel, Separator}
import de.sciss.audiowidgets.Transport
import de.sciss.synth.SynthGraph
import javax.swing.GroupLayout
import language.reflectiveCalls
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.swing.{DoubleSpinnerView, StringFieldView}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing._
import de.sciss.icons.raphael
import scala.concurrent.stm.Ref

object SonificationViewImpl {
  def apply[S <: Sys[S]](sonification: Obj.T[S, Sonification.Elem])
                        (implicit tx: S#Tx, workspace: Workspace[S]): SonificationView[S] = {
    implicit val undoMgr = new UndoManagerImpl {
      protected var dirty: Boolean = false
    }
    val sonifH    = tx.newHandle(sonification)
    val nameView  = sonification.attr.expr[String](Keys.attrName).map { expr =>
      import workspace.cursor
      StringFieldView(expr, "Name")
    }

    val sonifView = AuralWorkspaceHandler.instance.view[S, workspace.I](workspace).view(sonification)
    val p         = sonification.elem.peer.patch

    val res = new Impl[S](sonifH, sonifView, nameView) {
      val auralObserver = sonifView.react { implicit tx => upd =>
        deferTx(auralChange(upd))
      }

      val graphObserver = p.graph.changed.react { implicit tx => upd =>
        updateGraph(upd.now)
      }
    }
    val sonifState = sonifView.state
    deferTx {
      res.guiInit(sonifState)
    }

    val graph0  = p.graph.value
    res.updateGraph(graph0)

    res
  }

  private abstract class Impl[S <: Sys[S]](sonifH: stm.Source[S#Tx, Obj.T[S, Sonification.Elem]],
                                        sonifView: AuralSonification[S],
                                        nameView: Option[StringFieldView[S]])(implicit val workspace: Workspace[S],
                                                                              val undoManager: UndoManager)
    extends SonificationView[S] with ComponentHolder[Component] {

    protected def auralObserver: Disposable[S#Tx]
    protected def graphObserver: Disposable[S#Tx]

    def sonification(implicit tx: S#Tx): Obj.T[S, Sonification.Elem] = sonifH()

    private var pMapping : FlowPanel = _
    private var pControls: FlowPanel = _
    private var transportButtons: Component with Transport.ButtonStrip = _
    private var timerPrepare: javax.swing.Timer = _

    private val graphDisposables = Ref(Vec.empty[Disposable[S#Tx]])

    final protected def auralChange(upd: AuralSonification.Update): Unit = {
      requireEDT()
      val ggStop      = transportButtons.button(Transport.Stop).get
      val ggPlay      = transportButtons.button(Transport.Play).get
      ggStop.selected = upd == AuralSonification.Stopped
      ggPlay.selected = upd == AuralSonification.Playing || upd == AuralSonification.Preparing
      if (upd == AuralSonification.Preparing) timerPrepare.restart() else timerPrepare.stop()
    }

    final def updateGraph(g: SynthGraph)(implicit tx: S#Tx): Unit = {
      val sonif = sonifH()

      // ---- sources/mapping tx ----

      val mapping = sonif.elem.peer.sources
      val sources = g.sources.collect {
        case vr: graph.Var =>
          val key   = vr.name
          // vr.name
          // vr.higherRank
          val dimKeys = g.sources.collect {
            case graph.Dim(`vr`, dimKey) => dimKey
          }

          val view = SonificationSourceView(mapping, key, dimKeys)

          (key, view)
      }
      
      // ---- controls tx ----

      val controls    = sonif.elem.peer.controls
      val userValues  = g.sources.collect {
        case graph.UserValue(key, default) =>
          val view = DoubleSpinnerView.fromMap(controls, key, default, key.capitalize)
          (key, view)
      }

      val newObs = sources.map(_._2) ++ userValues.map(_._2)
      graphDisposables.swap(newObs)(tx.peer).foreach(_.dispose())

      deferTx {
        // ---- sources/mapping gui ----

        val ggMap = sources.map { case (key, view) =>
          val lb    = new Label(s"$key:")
          val drop  = view.component
          (lb, drop)
        }

        val pMap = new GroupPanel {
          import GroupPanel.Element
          horizontal = Seq(
            Par(ggMap.map(tup => Element(tup._1)): _*),
            Par(ggMap.map(tup => Element(tup._2)): _*)
          )

          vertical = Seq(
            ggMap.map { tup =>
              Par(Baseline)(tup._1, tup._2)
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
          import GroupPanel.Element
          horizontal = Seq(
            Par(ggCtl.map(tup => Element(tup._1)): _*),
            Par(ggCtl.map(tup => Element(tup._2)): _*)
          )

          vertical = Seq(
            ggCtl.map { tup =>
              Par(Baseline)(tup._1, tup._2)
            }: _*
          )
        }
        pControls.contents.clear()
        pControls.contents += pCtl

        // ----

        component.revalidate()
      }
    }

    final def guiInit(initState: AuralSonification.Update): Unit = {
      // ---- Header ----
      val actionEditPatch = new Action(null) {
        def apply(): Unit = cursor.step { implicit tx =>
          val sonif = sonifH()
          PatchCodeWindow(sonif.elem.peer.patch)
        }
      }
      val ggEditPatch = GUI.toolButton(actionEditPatch, raphael.Shapes.Edit, tooltip = "Edit Patch")
      val cHead       = nameView.fold(Vec.empty[Component])(view => Vec(new Label("Name:"), view.component))
      val pHeader     = new FlowPanel(cHead :+ ggEditPatch: _*)

      // ---- Mapping ----

      pMapping = new FlowPanel()
      pMapping.border = Swing.TitledBorder(Swing.EmptyBorder(4), "Mapping")

      // ---- Controls ----

      pControls = new FlowPanel()
      pControls.border = Swing.TitledBorder(Swing.EmptyBorder(4), "Controls")

      // ---- Transport ----

      transportButtons = Transport.makeButtonStrip {
        import Transport._
        Seq(GoToBegin(tGotToBegin()), Stop(tStop()), Play(tPlay()), Loop(tLoop()))
      }
      timerPrepare = new javax.swing.Timer(100, Swing.ActionListener { _ =>
        val ggPlay      = transportButtons.button(Transport.Play).get
        ggPlay.selected = !ggPlay.selected  // flash button at 10 Hz while preparing sonification playback
      })
      timerPrepare.setRepeats(true)
      auralChange(initState)
      val pTransport = new FlowPanel(transportButtons)
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

    private def tGotToBegin(): Unit = {
      println("TODO: GoToBegin")
    }

    private def tStop(): Unit = cursor.step { implicit tx =>
      sonifView.stop()
    }

    private def tPlay(): Unit = cursor.step { implicit tx =>
      sonifView.play()
    }

    private def tLoop(): Unit = {
      println("TODO: Loop")
    }

    final def dispose()(implicit tx: S#Tx): Unit = {
      sonifView.stop()
      auralObserver.dispose()
      graphObserver.dispose()
      graphDisposables.swap(Vec.empty)(tx.peer).foreach(_.dispose())
      nameView.foreach(_.dispose())
      deferTx {
        timerPrepare.stop()
      }
    }
  }
}

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
import scala.swing.{Alignment, Swing, Orientation, BoxPanel, Label, FlowPanel, Component}
import de.sciss.synth.proc.Attribute
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.UndoManager
import de.sciss.swingplus.Separator
import de.sciss.audiowidgets.Transport
import de.sciss.synth.SynthGraph
import scalaswingcontrib.group.GroupPanel
import javax.swing.GroupLayout
import language.reflectiveCalls
import de.sciss.lucre.stm.Disposable

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

    val sonifView = AuralWorkspaceHandler.instance.view(workspace).view(sonification)

    val res = new Impl(workspace, sonifH, sonifView, nameView) {
      val auralObserver = sonifView.react { implicit tx => upd =>
        GUI.fromTx(auralChange(upd))
      }
    }
    val sonifState = sonifView.state
    GUI.fromTx {
      res.guiInit(sonifState)
    }

    val p       = sonification.patch
    val graph0  = p.graph.value
    res.updateGraph(graph0)

    res
  }

  private abstract class Impl[S <: Sys[S]](val workspace: Workspace[S], sonifH: stm.Source[S#Tx, Sonification[S]],
                                        sonifView: AuralSonification[S],
                                        nameView: Option[StringExprEditor[S]])(implicit val undoManager: UndoManager)
    extends SonificationView[S] with ComponentHolder[Component] {

    protected def auralObserver: Disposable[S#Tx]

    def sonification(implicit tx: S#Tx): Sonification[S] = sonifH()

    private var pMapping : FlowPanel = _
    private var pControls: FlowPanel = _
    private var transportButtons: Component with Transport.ButtonStrip = _
    private var timerPrepare: javax.swing.Timer = _

    // XXX TODO: private val graphDisposables = STMRef[Vec[Disposable[S#Tx]]]

    final protected def auralChange(upd: AuralSonification.Update): Unit = {
      GUI.requireEDT()
      val ggStop      = transportButtons.button(Transport.Stop).get
      val ggPlay      = transportButtons.button(Transport.Play).get
      ggStop.selected = upd == AuralSonification.Stopped
      ggPlay.selected = upd == AuralSonification.Playing || upd == AuralSonification.Preparing
      if (upd == AuralSonification.Preparing) timerPrepare.restart() else timerPrepare.stop()
    }

    final def updateGraph(g: SynthGraph)(implicit tx: S#Tx): Unit = {
      val sonif = sonifH()

      // XXX TODO: graphDisposables.swap(Vec.empty).foreach(_.dispose())

      // ---- sources/mapping tx ----

      val mapping = sonif.sources
      val sources = g.sources.collect {
        case vr: graph.Var =>
          val key   = vr.name
          // vr.name
          // vr.higherRank
          val dimKeys = g.sources.collect {
            case graph.Dim(`vr`, dimKey) => dimKey
          }

          val view = SonificationSourceView(workspace, mapping, key, dimKeys)

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

    final def guiInit(initState: AuralSonification.Update): Unit = {
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
      auralObserver.dispose()
      sonifView.stop()
      nameView.foreach(_.dispose())
      GUI.fromTx {
        timerPrepare.stop()
      }
    }
  }
}

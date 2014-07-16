/*
 *  SonificationViewImpl.scala
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
package impl

import java.awt.Color

import de.sciss.lucre.synth.Sys
import at.iem.sysson.sound.{Keys, Sonification}
import de.sciss.lucre.stm
import de.sciss.synth.proc.{Transport, Obj}
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.{OptionPane, UndoManager}
import de.sciss.swingplus.{GroupPanel, Separator}
import de.sciss.audiowidgets.{Transport => GUITransport}
import de.sciss.synth.SynthGraph
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{DoubleSpinnerView, StringFieldView, deferTx}
import de.sciss.icons.raphael
import scala.concurrent.stm.Ref
import scala.swing.event.ButtonClicked
import scala.swing.{Action, Orientation, BoxPanel, ToggleButton, Swing, Alignment, Label, FlowPanel, Component}
import scala.util.control.NonFatal
import de.sciss.model.impl.ModelImpl
import de.sciss.lucre.matrix.gui.MatrixView
import de.sciss.mellite.{Mellite, Workspace}
import de.sciss.mellite.gui.{AttrMapFrame, CodeFrame, GUI}

object SonificationViewImpl {
  def apply[S <: Sys[S]](sonification: Obj.T[S, Sonification.Elem])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): SonificationView[S] = {
    implicit val undoMgr = new UndoManagerImpl {
      protected var dirty: Boolean = false
    }
    val sonifH    = tx.newHandle(sonification)
    val nameView  = sonification.attr.expr[String](Keys.attrName).map { expr =>
      // import workspace.cursor
      StringFieldView(expr, "Name")
    }

    val transport = Transport[S](Mellite.auralSystem)
    transport.addObject(sonification)
    val p         = sonification.elem.peer.proc

    val res = new Impl[S, workspace.I](sonifH, transport, nameView)(workspace, undoMgr, cursor) {
      val auralObserver = transport.react { implicit tx => upd =>
        deferTx(auralChange(upd))
      }

      val graphObserver = p.elem.peer.graph.changed.react { implicit tx => upd =>
        updateGraph(upd.now)
      }
    }
    // val sonifState = sonifView.state
    deferTx {
      res.guiInit(/* sonifState */)
    }

    val graph0  = p.elem.peer.graph.value
    res.updateGraph(graph0)

    res
  }

  private abstract class Impl[S <: Sys[S], I1 <: Sys[I1]](sonifH: stm.Source[S#Tx, Obj.T[S, Sonification.Elem]],
                                                          transport: Transport[S],
                                        nameView: Option[StringFieldView[S]])(implicit val workspace: Workspace[S] { type I = I1 },
                                                                              val undoManager: UndoManager,
                                                                              val cursor: stm.Cursor[S])
    extends SonificationView[S] with ComponentHolder[Component] with ModelImpl[SonificationView.Update] {

    impl =>

    protected def auralObserver: Disposable[S#Tx]
    protected def graphObserver: Disposable[S#Tx]

    def sonification(implicit tx: S#Tx): Obj.T[S, Sonification.Elem] = sonifH()

    private var pMapping : FlowPanel = _
    private var pControls: FlowPanel = _
    private var transportButtons: Component with GUITransport.ButtonStrip = _
    private var timerPrepare: javax.swing.Timer = _

    private val graphDisposables = Ref(Vec.empty[Disposable[S#Tx]])

    private var ggMute: ToggleButton = _

    private var ggElapsed: ElapsedBar = _

    final protected def auralChange(upd: Transport.Update[S] /* AuralSonificationOLD.Update */): Unit = upd match {
      //      case AuralSonificationOLD.Elapsed(dim, ratio, value) =>
      //        // println(f"$dim ${p * 100}%1.0f")
      //        ggElapsed.value = ratio

      case _ =>
        val ggStop      = transportButtons.button(GUITransport.Stop ).get
        val ggPlay      = transportButtons.button(GUITransport.Play ).get
        val stopped     = upd.isInstanceOf[Transport.Stop[S]] // AuralSonificationOLD.Stopped
        ggStop.selected = stopped
        ggPlay.selected = upd.isInstanceOf[Transport.Play[S]] // AuralSonificationOLD.Playing || upd == AuralSonificationOLD.Preparing
        // ggElapsed.textVisible = !stopped
          if (stopped) {
          ggMute.selected = false
          val ggPause     = transportButtons.button(GUITransport.Pause).get
          ggPause.selected = false
        }
        /* if (upd == AuralSonificationOLD.Preparing) timerPrepare.restart() else */ timerPrepare.stop()
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
          view.matrixView.addListener {
            case MatrixView.Resized => dispatch(SonificationView.Resized)
          }
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

    final def guiInit(/* initState: AuralSonificationOLD.Update */): Unit = {
      // ---- Header ----
      val actionEditProcAttr = new Action(null) {
        def apply(): Unit = cursor.step { implicit tx =>
          val sonif = sonifH()
          AttrMapFrame(sonif.elem.peer.proc)
        }
      }
      val actionEditProcGraph = new Action(null) {
        def apply(): Unit = cursor.step { implicit tx =>
          val sonif = sonifH()
          // PatchCodeWindow(sonif.elem.peer.proc)
          CodeFrame.proc(sonif.elem.peer.proc)
        }
      }

      val ggEditProcAttr  = GUI.toolButton(actionEditProcAttr , raphael.Shapes.Wrench, tooltip = "Edit Proc Attributes")
      val ggEditProcGraph = GUI.toolButton(actionEditProcGraph, raphael.Shapes.Edit  , tooltip = "Edit Synth Graph"    )
      // ggEditProcAttr .focusable = false
      // ggEditProcGraph.focusable = false
      val cHead           = nameView.fold(Vec.empty[Component])(view => Vec(new Label("Name:"), view.component))
      val pHeader         = new FlowPanel(cHead :+ ggEditProcAttr :+ ggEditProcGraph: _*)

      // ---- Mapping ----

      pMapping = new FlowPanel()
      pMapping.border = Swing.TitledBorder(Swing.EmptyBorder(4), "Mapping")

      // ---- Controls ----

      pControls = new FlowPanel()
      pControls.border = Swing.TitledBorder(Swing.EmptyBorder(4), "Controls")

      // ---- Transport ----

      transportButtons = GUITransport.makeButtonStrip {
        import GUITransport._
        Seq(/* GoToBegin(tGotToBegin()), */ Stop(tStop()), Pause(tPause()), Play(tPlay()) /*, Loop(tLoop()) */)
      }
      timerPrepare = new javax.swing.Timer(100, Swing.ActionListener { _ =>
        val ggPlay      = transportButtons.button(GUITransport.Play).get
        ggPlay.selected = !ggPlay.selected  // flash button at 10 Hz while preparing sonification playback
      })
      timerPrepare.setRepeats(true)

      ggElapsed = new ElapsedBar

      ggMute = new ToggleButton(null) { me =>
        listenTo(this)
        reactions += {
          case ButtonClicked(_) =>
            val muted = if (me.selected) 1f else 0
            println("TODO: Mute")
            //            workspace.inMemoryCursor.step { implicit tx =>
            //              sonifView.auralPresentation.group.foreach { g =>
            //                g.set(true, "$son_out" -> muted)
            //              }
            //            }
        }
        focusable = false
        icon          = raphael.Icon(extent = 20, fill = raphael.TexturePaint(24), shadow = raphael.WhiteShadow)(raphael.Shapes.Mute)
        disabledIcon  = raphael.Icon(extent = 20, fill = new Color(0, 0, 0, 0x7F), shadow = raphael.WhiteShadow)(raphael.Shapes.Mute)
        tooltip       = "Mute"
      }

      // auralChange(initState)

      val pTransport = new FlowPanel(Swing.HStrut(101), transportButtons, ggMute, ggElapsed)
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

    //    private def tGotToBegin(): Unit = {
    //      println("TODO: GoToBegin")
    //    }

    private def runGroup(state: Boolean)(implicit tx: S#Tx): Unit = {
      println("TODO: runGroup")
      //      sonifView.auralPresentation.group(workspace.inMemoryBridge(tx)).foreach { g =>
      //        g.run(audible = true, state = state)
      //      }
    }

    private def tStop(): Unit = {
      val ggPause   = transportButtons.button(GUITransport.Pause).get
      val isPausing = ggPause.selected
      cursor.step { implicit tx =>
        transport.toString
        // sonifView.stop()
        if (isPausing) runGroup(state = true)
      }
    }

    private def tPause(): Unit = {
      val ggPause   = transportButtons.button(GUITransport.Pause).get
      val isPausing = !ggPause.selected
      val isPlaying = cursor.step { implicit tx =>
        // val p = sonifView.state == AuralSonificationOLD.Playing
        val p = transport.isPlaying
        if (p) runGroup(state = !isPausing)
        p
      }
      if (isPlaying) ggPause.selected = isPausing
    }

    private def tPlay(): Unit = cursor.step { implicit tx =>
      try {
        transport.play()
        // sonifView.play()
      } catch {
        case NonFatal(e) =>
          val opt = OptionPane.message(
            message = s"<html><b>Sonification failed due to the following problem:</b><p>${e.getMessage}",
            messageType = OptionPane.Message.Error)
          opt.show(GUI.findWindow(component))
      }
    }

    //    private def tLoop(): Unit = {
    //      println("TODO: Loop")
    //    }

    final def dispose()(implicit tx: S#Tx): Unit = {
      transport.dispose()
      // sonifView.stop()
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

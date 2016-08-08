/*
 *  SonificationViewImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2016 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2016 Hanns Holger Rutz. All rights reserved.
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

import at.iem.sysson.sound.{AuralSonification, Sonification}
import de.sciss.audiowidgets.{TimelineModel, Transport => GUITransport}
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.{KeyStrokes, OptionPane, UndoManager, Window}
import de.sciss.file._
import de.sciss.icons.raphael
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, SpanLikeObj, StringObj}
import de.sciss.lucre.matrix.gui.MatrixView
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{CellView, DoubleSpinnerView, StringFieldView, deferTx}
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.edit.EditAttrMap
import de.sciss.mellite.gui.{ActionBounceTimeline, AttrMapFrame, CodeFrame, GUI}
import de.sciss.mellite.{Mellite, Workspace}
import de.sciss.model.impl.ModelImpl
import de.sciss.span.Span
import de.sciss.swingplus.{GroupPanel, Separator}
import de.sciss.synth.SynthGraph
import de.sciss.synth.proc.{ObjKeys, TimeRef, Timeline, Transport}

import scala.concurrent.stm.Ref
import scala.swing.event.{ButtonClicked, Key}
import scala.swing.{Action, Alignment, BoxPanel, Component, FlowPanel, Label, Orientation, ScrollPane, Swing, ToggleButton}
import scala.util.control.NonFatal

object SonificationViewImpl {
  def apply[S <: Sys[S]](sonification: Sonification[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): SonificationView[S] = {
    implicit val undoMgr = new UndoManagerImpl
    val sonifH    = tx.newHandle(sonification)
    val nameView  = sonification.attr.$[StringObj](ObjKeys.attrName).map { expr =>
      // import workspace.cursor
      StringFieldView(expr, "Name")
    }

    val p = sonification.proc

    val res = new Impl[S, workspace.I](sonifH, /* TTT transport, */ nameView)(workspace, undoMgr, cursor) {
      val graphObserver = p.graph.changed.react { implicit tx => upd =>
        updateGraph(upd.now)
      }
    }
    // val sonifState = sonifView.state
    deferTx {
      res.guiInit(/* sonifState */)
    }

    val graph0  = p.graph.value
    res.updateGraph(graph0)

    res
  }

  private final class TransportRef[S <: Sys[S]](val transport: Transport[S],
                                                val transportObserver: Disposable[S#Tx],
                                                val viewObserver: Option[Disposable[S#Tx]])
    extends Disposable[S#Tx] {

    def dispose()(implicit tx: S#Tx): Unit = {
      transport.stop()
      transport.dispose()
      transportObserver.dispose()
      viewObserver.foreach(_.dispose())
    }
  }

  private abstract class Impl[S <: Sys[S], I1 <: Sys[I1]](sonifH: stm.Source[S#Tx, Sonification[S]],
                                                          /* TTT transport: Transport[S], */
                                        nameView: Option[StringFieldView[S]])(implicit val workspace: Workspace[S] { type I = I1 },
                                                                              val undoManager: UndoManager,
                                                                              val cursor: stm.Cursor[S])
    extends SonificationView[S] with ComponentHolder[Component] with ModelImpl[SonificationView.Update] {

    impl =>

    protected def graphObserver: Disposable[S#Tx]

    def sonification(implicit tx: S#Tx): Sonification[S] = sonifH()

    private var pMapping : FlowPanel = _
    private var pControls: FlowPanel = _
    private var transportButtons: Component with GUITransport.ButtonStrip = _
    private var timerPrepare: javax.swing.Timer = _

    private val graphDisposables = Ref(Vec.empty[Disposable[S#Tx]])

    private var ggMute: ToggleButton = _

    private var ggElapsed: ElapsedBar = _

    private def mkTransport()(implicit tx: S#Tx, cursor: stm.Cursor[S], workspace: Workspace[S]): TransportRef[S] = {
      val t     = Transport[S](Mellite.auralSystem)
      val sonif = sonification
      t.addObject(sonif)
      val observer = t.react { implicit tx => upd =>
        deferTx(auralChange(upd))
      }
      val elapsedOpt = t.getView(sonif).collect {
        case as: AuralSonification[S] =>
          as.status.react { implicit tx => {
            case e @ AuralSonification.Elapsed(_, ratio, dimValue) =>
              status() = e
              deferTx {
                ggElapsed.value = ratio
              }

            case AuralSonification.PlayFailed(cause) =>
              // status() = e
              deferTx {
                showPlayError(cause)
              }
          }}
      }
      new TransportRef(t, observer, elapsedOpt)
    }

    object status extends ObservableImpl[S, AuralSonification.Update] {
      def update(u: AuralSonification.Update)(implicit tx: S#Tx): Unit = fire(u)
    }

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
//          val ggPause     = transportButtons.button(GUITransport.Pause).get
//          ggPause.selected = false
        }
        /* if (upd == AuralSonificationOLD.Preparing) timerPrepare.restart() else */ timerPrepare.stop()
    }

    final def updateGraph(g: SynthGraph)(implicit tx: S#Tx): Unit = {
      val sonif = sonifH()

      // ---- sources/mapping tx ----

      // val mapping = sonif.elem.peer.sources
      val sources = g.sources.collect {
        case vr: graph.Var =>
          val key   = vr.name
          // vr.name
          // vr.higherRank
          val dimKeys = g.sources.collect {
            case graph.Dim(`vr`, dimKey) => dimKey
          }

          val view = SonificationSourceView(impl, key, dimKeys)

          (key, view)
      }

      // ---- controls tx ----

      val controls    = sonif.controls
      val userValues  = g.sources.collect {
        case graph.UserValue(key, default) =>
          // val view = DoubleSpinnerView.fromMap(controls, key, default, key.capitalize)
          implicit val doubleEx = DoubleObj
          val cell      = CellView.exprMap[S, String, Double, DoubleObj](controls, key)
          val view      = DoubleSpinnerView.optional[S](cell, name = key.capitalize, default = Some(default))
          (key, view)
      }

      val newObs = sources.map(_._2) ++ userValues.map(_._2)
      graphDisposables.swap(newObs)(tx.peer).foreach(_.dispose())

      deferTx {
        // ---- sources/mapping gui ----

        val ggMap = sources.zipWithIndex.map { case ((key, view), idx) =>
          val lb    = new Label(s"$key:")
          if (idx > 0) lb.border = Swing.EmptyBorder(32, 0, 0, 0)
          val drop  = view.component
          view.matrixView.addListener {
            case MatrixView.Resized => dispatch(SonificationView.Resized)
          }
          (lb, drop)
        }

        val pMap = new GroupPanel {
          import de.sciss.swingplus.GroupPanel.Element
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
          AttrMapFrame(sonif.proc)
        }
      }
      val actionEditProcGraph = new Action(null) {
        def apply(): Unit = cursor.step { implicit tx =>
          val sonif = sonifH()
          // PatchCodeWindow(sonif.elem.peer.proc)
          import de.sciss.mellite.Mellite.compiler
          CodeFrame.proc(sonif.proc)
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
        import de.sciss.audiowidgets.Transport._
        Seq(/* GoToBegin(tGotToBegin()), */ Stop(tStop()), /* Pause(tPause()), */ Play(tPlay()) /*, Loop(tLoop()) */)
      }
      timerPrepare = new javax.swing.Timer(100, Swing.ActionListener { _ =>
        val ggPlay      = transportButtons.button(GUITransport.Play).get
        ggPlay.selected = !ggPlay.selected  // flash button at 10 Hz while preparing sonification playback
      })
      timerPrepare.setRepeats(true)

      ggElapsed = new ElapsedBar

      // XXX TODO - should be regular button, and has to listen to model changes
      ggMute = new ToggleButton(null) { me =>
        listenTo(this)
        reactions += {
          case ButtonClicked(_) =>
            val muted = me.selected
            implicit val cursor = impl.cursor
            val edit = cursor /* workspace.inMemoryCursor */ .step { implicit tx =>
              // val imp = ExprImplicits[S]
              val value = BooleanObj.newConst[S](muted)
              val proc  = sonification.proc
              // Not sure that making an undoable edit is necessary, perhaps just overkill?
              EditAttrMap(if (muted) "Mute" else "Un-Mute", obj = proc, key = ObjKeys.attrMute, value = Some(value))
              //              sonifView.auralPresentation.group.foreach { g =>
              //                g.set(true, "$son_out" -> muted)
              //              }
            }
            undoManager.add(edit)
        }
        focusable = false
        icon          = raphael.Icon(extent = 20, fill = raphael.TexturePaint(24), shadow = raphael.WhiteShadow)(raphael.Shapes.Mute)
        disabledIcon  = raphael.Icon(extent = 20, fill = new Color(0, 0, 0, 0x7F), shadow = raphael.WhiteShadow)(raphael.Shapes.Mute)
        tooltip       = "Mute"
      }

      // auralChange(initState)

      val pTransport = new FlowPanel(Swing.HStrut(101), transportButtons, ggMute, ggElapsed)
      pTransport.border = Swing.TitledBorder(Swing.EmptyBorder(4), "Transport")

      val box = new BoxPanel(Orientation.Vertical) {
        contents += pMapping
        contents += Separator()
        contents += pControls
      }
      component = new BoxPanel(Orientation.Vertical) {
        contents += pHeader
        contents += Separator()
        contents += new ScrollPane(box) { border = null }
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

    private val transportRef = Ref(Option.empty[TransportRef[S]])

    private def tStop(): Unit = {
      // val ggPause   = transportButtons.button(GUITransport.Pause).get
      val isPausing = false // ggPause.selected
      cursor.step { implicit tx =>
        // TTT
        // transport.stop()
        stopAndDisposeTransport()
        if (isPausing) runGroup(state = true)
      }
    }

    private def stopAndDisposeTransport()(implicit tx: S#Tx): Unit =
      transportRef.swap(None)(tx.peer).foreach(_.dispose())

//    private def tPause(): Unit = {
//      val ggPause   = transportButtons.button(GUITransport.Pause).get
//      val isPausing = !ggPause.selected
//      val isPlaying = cursor.step { implicit tx =>
//        // val p = sonifView.state == AuralSonificationOLD.Playing
//        val p = transportRef.get(tx.peer).exists(_.transport.isPlaying) // transport.isPlaying
//        if (p) runGroup(state = !isPausing)
//        p
//      }
//      if (isPlaying) ggPause.selected = isPausing
//    }

    private def tPlay(): Unit = cursor.step { implicit tx =>
      try {
        // TTT
        stopAndDisposeTransport()   // make sure that old object is disposed
        val ref = mkTransport()
        transportRef.set(Some(ref))(tx.peer)
        ref.transport.play()

      } catch {
        case NonFatal(e) => showPlayError(e)
      }
    }

    private def showPlayError(cause: Throwable): Unit = {
      val message = s"<html><b>Sonification failed:</b> <i>(${cause.getClass.getSimpleName})</i><p>${cause.getMessage}"
      val options = Seq("Ok", "Show Stack Trace")
      val opt = OptionPane(message = message, messageType = OptionPane.Message.Error,
        optionType = OptionPane.Options.YesNo, entries = options, initial = options.headOption)
      if (opt.show(Window.find(component)).id == 1) cause.printStackTrace()
    }

    //    private def tLoop(): Unit = {
    //      println("TODO: Loop")
    //    }

    final def dispose()(implicit tx: S#Tx): Unit = {
      // TTT
      // transport.dispose()
      // auralObserver.dispose()
      stopAndDisposeTransport()

      graphObserver.dispose()
      graphDisposables.swap(Vec.empty)(tx.peer).foreach(_.dispose())
      nameView.foreach(_.dispose())
      deferTx {
        timerPrepare.stop()
      }
    }

    object actionBounce extends Action("Export as Audio File...") {
      import ActionBounceTimeline.{DurationSelection, QuerySettings, performGUI, query1}

      private var settings = QuerySettings[S](
        realtime = true, span = Span(0L, (TimeRef.SampleRate * 10).toLong), channels = Vector(0 to 1)
      )

      accelerator = Some(KeyStrokes.menu1 + Key.B)

      def apply(): Unit = {
        val window  = Window.find(component)
        val setUpd  = if (settings.file.isDefined) settings else {
          settings.copy(file = nameView.map { v =>
            val name  = v.component.text
            val ext   = settings.spec.fileType.extension
            val child = s"$name.$ext"
            val parentOption = workspace.folder.flatMap(_.parentOption)
            parentOption.fold(file(child))(_ / child)
          })
        }
        val timelineModel = TimelineModel(Span(0L, (TimeRef.SampleRate * 10000).toLong), TimeRef.SampleRate)
        val (_settings, ok) = query1(setUpd, workspace, timelineModel, window = window,
          showImport = false, showTransform = false, showSelection = DurationSelection)
        settings = _settings
        _settings.file match {
          case Some(file) if ok =>
            // XXX TODO -- not cool having to create a throw away object (perhaps persistent)
            val groupEH = cursor.step { implicit tx =>
              val tl    = Timeline[S]
              val span  = _settings.span
              tl.add(SpanLikeObj.newConst(span), sonifH())
              val tlObj = tl // Obj(Timeline.Elem(tl))
              tx.newHandle(tlObj)
            }
            import Mellite.compiler
            performGUI(workspace, _settings, groupEH, file, window = window)
          case _ =>
        }
      }
    }
  }
}
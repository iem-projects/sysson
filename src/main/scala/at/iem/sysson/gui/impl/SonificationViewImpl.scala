/*
 *  SonificationViewImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2017 Hanns Holger Rutz. All rights reserved.
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

import at.iem.sysson.sound.{AuralSonification, Sonification}
import de.sciss.audiowidgets.{Transport => GUITransport}
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.{OptionPane, UndoManager, Window}
import de.sciss.file._
import de.sciss.icons.raphael
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, StringObj}
import de.sciss.lucre.matrix.gui.MatrixView
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{CellView, DoubleSpinnerView, StringFieldView, deferTx}
import de.sciss.lucre.synth.Sys
import de.sciss.{equal, mellite}
import de.sciss.mellite.Mellite
import de.sciss.mellite.gui.edit.EditAttrMap
import de.sciss.mellite.gui.{ActionBounceTimeline, AttrMapFrame, CodeFrame, GUI, MarkdownRenderFrame}
import de.sciss.model.impl.ModelImpl
import de.sciss.swingplus.{GroupPanel, Separator}
import de.sciss.synth.SynthGraph
import de.sciss.synth.proc.{AuralObj, AuralView, Markdown, ObjKeys, Transport, Workspace}

import scala.concurrent.stm.Ref
import scala.swing.event.ButtonClicked
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

    val p       = sonification.proc
    val hasHelp = sonification.attr.contains(Sonification.attrHelp)

    val res = new Impl[S, workspace.I](sonifH, nameView, hasHelp = hasHelp)(workspace, undoMgr, cursor) {
      val graphObserver: Disposable[S#Tx] =
        p.graph.changed.react { implicit tx => upd =>
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

  private final class TransportRef[S <: Sys[S]](val transport         : Transport[S],
                                                val transportObserver : Disposable[S#Tx],
                                                val viewObserver      : Ref[Option[Disposable[S#Tx]]],
                                                val elapsedObserver   : Ref[Option[Disposable[S#Tx]]]
                                               )
    extends Disposable[S#Tx] {

    def dispose()(implicit tx: S#Tx): Unit = {
      transportObserver.dispose()
      viewObserver   .swap(None)(tx.peer).foreach(_.dispose())
      elapsedObserver.swap(None)(tx.peer).foreach(_.dispose())
      transport.stop()
      transport.dispose()
    }
  }

  private abstract class Impl[S <: Sys[S], I1 <: Sys[I1]](sonifH: stm.Source[S#Tx, Sonification[S]],
                                                          nameView: Option[StringFieldView[S]], hasHelp: Boolean)
                                                         (implicit val workspace: Workspace[S] { type I = I1 },
                                                          val undoManager: UndoManager,
                                                          val cursor: stm.Cursor[S])
    extends SonificationView[S] with ComponentHolder[Component] with ModelImpl[SonificationView.Update] {

    impl =>

    protected def graphObserver: Disposable[S#Tx]

    def sonification(implicit tx: S#Tx): Sonification[S] = sonifH()

    private[this] var pMapping : FlowPanel = _
    private[this] var pControls: FlowPanel = _
    private[this] var transportButtons: Component with GUITransport.ButtonStrip = _
    private[this] var timerPrepare: javax.swing.Timer = _

    private[this] val graphDisposables = Ref(Vec.empty[Disposable[S#Tx]])

    private[this] var ggMute   : ToggleButton = _
    private[this] var ggElapsed: ElapsedBar   = _

    private def mkTransport()(implicit tx: S#Tx, cursor: stm.Cursor[S], workspace: Workspace[S]): TransportRef[S] = {
      val t     = Transport[S](Mellite.auralSystem)
      val sonif = sonification
      val aObsR = Ref(Option.empty[Disposable[S#Tx]])
      val aStatR= Ref(Option.empty[Disposable[S#Tx]])

      def viewState(state: AuralView.State)(implicit tx: S#Tx): Unit = {
        val tp = t.isPlaying
        deferTx(auralChange(state, transportPlaying = tp))
      }

      def viewAdded(view: AuralObj[S])(implicit tx: S#Tx): Unit = {
        val aObs = view.react { implicit tx => viewState(_)(tx) }
        aObsR.swap(Some(aObs))(tx.peer).foreach(_.dispose())
        val aStat = view match {
          case as: AuralSonification[S] =>
            val obs = as.status.react { implicit tx => {
              case e @ AuralSonification.Elapsed(_, ratio, _ /* dimValue */) =>
                status() = e
                deferTx {
                  ggElapsed.value = ratio
                }

              case AuralSonification.PlayFailed(cause) =>
                deferTx {
                  showPlayError(cause)
                }
            }}
            Some(obs)
          case _ => None
        }
        aStatR.swap(aStat)(tx.peer).foreach(_.dispose())
        viewState(view.state)
      }

      val observer = t.react { implicit tx => {
        // deferTx(auralChange(upd))
        case Transport.ViewAdded(_, view) => viewAdded(view)
        case _ =>
      }}
      t.addObject(sonif)

      new TransportRef(t, observer, aObsR, aStatR)
    }

    object status extends ObservableImpl[S, AuralSonification.Update] {
      def update(u: AuralSonification.Update)(implicit tx: S#Tx): Unit = fire(u)
    }

    private def auralChange(state: AuralView.State, transportPlaying: Boolean): Unit = {
//      println(s"STATE $state")
      val ggStop      = transportButtons.button(GUITransport.Stop).get
      val ggPlay      = transportButtons.button(GUITransport.Play).get
      import equal.Implicits._
      val stopped     = state === AuralView.Stopped // upd.isInstanceOf[Transport.Stop[S]]
      val playing     = state === AuralView.Playing
      val preparing   = !playing && transportPlaying // state === AuralView.Preparing
      ggStop.selected = stopped
      ggPlay.selected = playing
      if (preparing) timerPrepare.restart() else timerPrepare.stop()
      if (stopped) {
        ggMute.selected = false
//          val ggPause     = transportButtons.button(GUITransport.Pause).get
//          ggPause.selected = false
      }
    }

    final def updateGraph(g: SynthGraph)(implicit tx: S#Tx): Unit = {
      val sonif = sonifH()

      // ---- sources/mapping tx ----

      // val mapping = sonif.elem.peer.sources
      val sources = g.sources.collect {
        case vr: graph.Var =>
          val key     = vr.name
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
      val actionEditProcAttr = Action(null) {
        cursor.step { implicit tx =>
          val sonif = sonifH()
          AttrMapFrame(sonif.proc)
        }
      }
      val actionEditProcGraph = Action(null) {
        cursor.step { implicit tx =>
          val sonif = sonifH()
          import de.sciss.mellite.Mellite.compiler
          CodeFrame.proc(sonif.proc)
        }
      }

      val ggEditProcAttr  = GUI.toolButton(actionEditProcAttr , raphael.Shapes.Wrench, tooltip = "Edit Proc Attributes")
      val ggEditProcGraph = GUI.toolButton(actionEditProcGraph, raphael.Shapes.Edit  , tooltip = "Edit Synth Graph"    )
      val cHead0          = nameView.fold(Vec.empty[Component])(view => Vec(new Label("Name:"), view.component))
      val cHead           = if (!hasHelp) cHead0 else {
        val actionHelp = Action(null) {
          cursor.step { implicit tx =>
            val sonif = sonifH()
            sonif.attr.$[Markdown](Sonification.attrHelp).foreach { md =>
              MarkdownRenderFrame(md)
            }
          }
        }
        val ggHelp = GUI.toolButton(actionHelp, raphael.Shapes.Help  , tooltip = "View Documentation"  )
        cHead0 :+ ggHelp
      }
      val pHeader = new FlowPanel(cHead :+ ggEditProcAttr :+ ggEditProcGraph: _*)

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
        ggPlay.selected = !ggPlay.selected  // flash button at 5 Hz while preparing sonification playback
      })
      timerPrepare.setRepeats(true)

      ggElapsed = new ElapsedBar

      // XXX TODO - should be regular button, and has to listen to model changes
//      ggMute = GUI.toolButton(Action(null){}, mellite.gui.Shapes.Mute, "Mute")
      ggMute = new ToggleButton(null) { me =>
        listenTo(this)
        reactions += {
          case ButtonClicked(_) =>
            val muted = me.selected
            implicit val cursor = impl.cursor
            val edit = cursor /* workspace.inMemoryCursor */ .step { implicit tx =>
              val value = BooleanObj.newConst[S](muted)
              val proc  = sonification.proc
              // Not sure that making an undoable edit is necessary, perhaps just overkill?
              EditAttrMap(if (muted) "Mute" else "Un-Mute", obj = proc, key = ObjKeys.attrMute, value = Some(value))
            }
            undoManager.add(edit)
        }
        focusable = false
        icon          = GUI.iconNormal  (mellite.gui.Shapes.Mute)
        disabledIcon  = GUI.iconDisabled(mellite.gui.Shapes.Mute)
        tooltip       = "Mute"
      }

      // auralChange(initState)

      val pTransport = new FlowPanel(Swing.HStrut(101), transportButtons, ggMute, ggElapsed)

      val box = new BoxPanel(Orientation.Vertical) {
        contents += pMapping
        contents += Separator()
        contents += pControls
      }
      val scroll = new ScrollPane(box)
      scroll.peer.putClientProperty("styleId", "undecorated")

      auralChange(AuralView.Stopped, transportPlaying = false)

      component = new BoxPanel(Orientation.Vertical) {
        contents += pHeader
        contents += Separator()
        contents += scroll
        contents += Separator()
        contents += pTransport
      }
    }

//    private def runGroup(state: Boolean)(implicit tx: S#Tx): Unit = {
//      println("TODO: runGroup")
//      //      sonifView.auralPresentation.group(workspace.inMemoryBridge(tx)).foreach { g =>
//      //        g.run(audible = true, state = state)
//      //      }
//    }

    private val transportRef = Ref(Option.empty[TransportRef[S]])

    private def tStop(): Unit = {
      // val ggPause   = transportButtons.button(GUITransport.Pause).get
//      val isPausing = false // ggPause.selected
      cursor.step { implicit tx =>
        // TTT
        // transport.stop()
        stopAndDisposeTransport()
//        if (isPausing) runGroup(state = true)
      }
      auralChange(AuralView.Stopped, transportPlaying = false)
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
      stopAndDisposeTransport()

      graphObserver.dispose()
      graphDisposables.swap(Vector.empty)(tx.peer).foreach(_.dispose())
      nameView.foreach(_.dispose())
      deferTx {
        timerPrepare.stop()
      }
    }

    object actionBounce extends ActionBounceTimeline(this, sonifH) {
      import ActionBounceTimeline.{DurationSelection, Selection}

      override protected def selectionType  : Selection = DurationSelection

      override protected def defaultRealtime(implicit tx: S#Tx): Boolean = true

      override protected def defaultFile(implicit tx: S#Tx): File =
        nameView.fold(super.defaultFile) { v =>
          val name  = v.component.text
//          val ext   = settings.spec.fileType.extension
          val child = name // s"$name.$ext" -- note: recallSettings will automatically replace the extension
          val parentOption = workspace.folder.flatMap(_.parentOption)
          parentOption.fold(file(child))(_ / child)
        }
    }
  }
}
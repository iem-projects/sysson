/*
 *  SonificationObjView.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 Hanns Holger Rutz. All rights reserved.
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

import at.iem.sysson.sound.Sonification
import at.iem.sysson.sound.impl.SonificationImpl.SonificationElemImpl
import de.sciss.desktop
import de.sciss.desktop.OptionPane
import de.sciss.icons.raphael
import de.sciss.lucre.bitemp.{SpanLike => SpanLikeEx}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.mellite.Workspace
import de.sciss.mellite.gui.impl.timeline.TimelineObjViewImpl
import de.sciss.mellite.gui.impl.{ListObjViewImpl, ObjViewImpl}
import de.sciss.mellite.gui.{ListObjView, TimelineObjView}
import de.sciss.span.SpanLike
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{FadeSpec, Obj}
import org.scalautils.TypeCheckedTripleEquals

import scala.swing.{Component, Label}

object SonificationObjView extends ListObjView.Factory with TimelineObjView.Factory {
  type E[S <: Sys[S]] = Sonification.Elem[S]
  final val prefix  = "Sonification"
  def humanName     = prefix
  final val icon    = ObjViewImpl.raphaelIcon(raphael.Shapes.Feed)
  final val typeID  = SonificationElemImpl.typeID
  def category      = SwingApplication.categSonification

  def hasMakeDialog: Boolean = true

  private lazy val _init: Unit = {
    ListObjView.addFactory(this)
    TimelineObjView.addFactory(this)
  }

  def init(): Unit = _init

  def mkListView[S <: SSys[S]](obj: Obj.T[S, E])(implicit tx: S#Tx): ListObjView[S] = {
    val son       = obj.elem.peer
    val procName  = son.proc.name
    new SonificationObjView.ListImpl(tx.newHandle(obj), value = new Value(procName)).initAttrs(obj)
  }

  def mkTimelineView[S <: SSys[S]](id: S#ID, span: Expr[S, SpanLike], obj: Sonification.Obj[S],
                                  context: TimelineObjView.Context[S])
                        (implicit tx: S#Tx): TimelineObjView[S] = {
    import SpanLikeEx.serializer
    val son       = obj.elem.peer
    val procName  = son.proc.name
    val res = new TimelineImpl(span = tx.newHandle(span), obj = tx.newHandle(obj),
      value = new Value(procName)).initAttrs(span, obj)
    TimelineObjViewImpl.initGainAttrs(span, obj, res)
    TimelineObjViewImpl.initMuteAttrs(span, obj, res)
    TimelineObjViewImpl.initFadeAttrs(span, obj, res)
    res
  }

  type Config[S <: Sys[S]] = String

  def initMakeDialog[S <: SSys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                              (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
    val opt = OptionPane.textInput(message = "Enter Sonification Name:",
      messageType = OptionPane.Message.Question, initial = "Sonification")
    opt.title = "Add Sonification"
    val res = opt.show(window)
    res
  }

  def makeObj[S <: SSys[S]](name: String)(implicit tx: S#Tx): List[Obj[S]] = {
    val elem  = Sonification.Elem(Sonification[S])
    val obj   = Obj(elem)
    obj.name  = name
    obj :: Nil
  }

  private final class Value(procName: String) {
    override def toString: String = {
      import TypeCheckedTripleEquals._
      if (procName === "<unnamed>") "" else s"proc: $procName"
    }
  }

  private trait Impl[S <: SSys[S]]
    extends ObjViewImpl.Impl[S] with ListObjViewImpl.NonEditable[S] with ListObjView[S] {

    override def obj: stm.Source[S#Tx, Sonification.Obj[S]]

    def factory = SonificationObjView
    def prefix  = SonificationObjView.prefix

    def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean = update match {
      case Sonification.Update(_, ch) => false
        // XXX TODO - I don't see how the patch obj's name changes are propagated
        //        ch.exists {
        //          case Sonification.PatchChange(Change())
        //        }
    }

    def isViewable = true

    def openView(parent: Option[Window[S]])
                (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
      val frame = SonificationFrame(obj())
      Some(frame)
    }

    def configureRenderer(label: Label): Component = {
      val txt    = value.toString
      label.text = txt
      label
    }
  }

  private final class ListImpl[S <: SSys[S]](val obj: stm.Source[S#Tx, Obj.T[S, Sonification.Elem]], var value: Value)
    extends Impl[S]

  private final class TimelineImpl[S <: SSys[S]](val span: stm.Source[S#Tx, Expr[S, SpanLike]],
                                        val obj : stm.Source[S#Tx, Sonification.Obj[S]], val value: Value)
    extends Impl[S] with TimelineObjViewImpl.BasicImpl[S]
    with TimelineObjView.HasMute
    with TimelineObjView.HasGain
    with TimelineObjView.HasFade { self =>

    override def toString() = s"SonificationTimelineView($name, $spanValue)"

    var gain        : Double          = _
    var muted       : Boolean         = _
    var fadeIn      : FadeSpec        = _
    var fadeOut     : FadeSpec        = _
  }
}
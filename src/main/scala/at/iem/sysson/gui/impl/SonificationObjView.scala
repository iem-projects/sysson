/*
 *  SonificationObjView.scala
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

import javax.swing.Icon

import at.iem.sysson.sound.Sonification
import de.sciss.desktop
import de.sciss.desktop.OptionPane
import de.sciss.icons.raphael
import de.sciss.lucre.expr.{SpanLikeObj, StringObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.mellite.gui.impl.timeline.TimelineObjViewBasicImpl
import de.sciss.mellite.gui.impl.{ListObjViewImpl, ObjViewImpl}
import de.sciss.mellite.gui.{ListObjView, ObjView, TimelineObjView}
import de.sciss.synth.proc.{FadeSpec, ObjKeys, Workspace}

object SonificationObjView extends ListObjView.Factory with TimelineObjView.Factory {
  type E[S <: Sys[S]]   = Sonification[S]
  final val prefix      = "Sonification"
  def humanName: String = prefix
  final val icon: Icon  = ObjViewImpl.raphaelIcon(raphael.Shapes.Feed)
  final val typeID: Int = Sonification.typeID
  def category: String  = SwingApplication.categSonification

  def tpe: Obj.Type = Sonification

  def hasMakeDialog: Boolean = true

  private lazy val _init: Unit = {
    ListObjView.addFactory(this)
    TimelineObjView.addFactory(this)
  }

  def init(): Unit = _init

  def mkListView[S <: SSys[S]](obj: E[S])(implicit tx: S#Tx): ListObjView[S] = {
    // val son       = obj.elem.peer
    // val procName  = son.proc.name
    new SonificationObjView.ListImpl(tx.newHandle(obj) /* , value = new Value(procName) */).initAttrs(obj)
  }

  def mkTimelineView[S <: SSys[S]](id: S#ID, span: SpanLikeObj[S], obj: Sonification[S],
                                  context: TimelineObjView.Context[S])
                        (implicit tx: S#Tx): TimelineObjView[S] = {
    // val son       = obj.elem.peer
    // val procName  = son.proc.name
    val res = new TimelineImpl(objH = tx.newHandle(obj) /*,
      value = new Value(procName) */).initAttrs(id, span, obj)

    ???
//    TimelineObjViewImpl.initGainAttrs(span, obj, res)
//    TimelineObjViewImpl.initMuteAttrs(span, obj, res)
//    TimelineObjViewImpl.initFadeAttrs(span, obj, res)
    res
  }

  type Config[S <: Sys[S]] = String

  def initMakeDialog[S <: SSys[S]](workspace: Workspace[S], window: Option[desktop.Window])(ok: Config[S] => Unit)
                              (implicit cursor: stm.Cursor[S]): Unit = {
    val opt = OptionPane.textInput(message = "Enter Sonification Name:",
      messageType = OptionPane.Message.Question, initial = "Sonification")
    opt.title = "Add Sonification"
    val res = opt.show(window)
    res.foreach(ok(_))
  }

  def makeObj[S <: SSys[S]](name: String)(implicit tx: S#Tx): List[Obj[S]] = {
    val elem  = Sonification[S]
    val obj   = elem // Obj(elem)
    val nameObj = StringObj.newVar(StringObj.newConst[S](name))
    // share the name, so it appears in the proc editor as well
    obj     .attr.put(ObjKeys.attrName, nameObj)
    obj.proc.attr.put(ObjKeys.attrName, nameObj)
    obj :: Nil
  }

//  private final class Value(procName: String) {
//    override def toString: String = {
//      import TypeCheckedTripleEquals._
//      if (procName === "<unnamed>") "" else s"proc: $procName"
//    }
//  }

  private trait Impl[S <: SSys[S]]
    extends ObjViewImpl.Impl[S]
    with ListObjViewImpl.NonEditable[S]
    with ListObjViewImpl.EmptyRenderer[S]
    with ListObjView[S] {

    override def objH: stm.Source[S#Tx, Sonification[S]]

    override def obj(implicit tx: S#Tx): Sonification[S] = objH()

    def factory: ObjView.Factory  = SonificationObjView
    def prefix: String            = SonificationObjView.prefix

    //    def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean = update match {
    //      case Sonification.Update(_, ch) => false
    //        // XXX TODO - I don't see how the patch obj's name changes are propagated
    //        //        ch.exists {
    //        //          case Sonification.PatchChange(Change())
    //        //        }
    //    }

    def isViewable = true

    def openView(parent: Option[Window[S]])
                (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
      val frame = SonificationFrame(obj)
      Some(frame)
    }

//    def configureRenderer(label: Label): Component = {
//      val txt    = value.toString
//      label.text = txt
//      label
//    }
  }

  private final class ListImpl[S <: SSys[S]](val objH: stm.Source[S#Tx, Sonification[S]] /*, var value: Value */)
    extends Impl[S]

  private final class TimelineImpl[S <: SSys[S]](val objH: stm.Source[S#Tx, Sonification[S]] /* , val value: Value */)
    extends Impl[S] with TimelineObjViewBasicImpl[S]
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
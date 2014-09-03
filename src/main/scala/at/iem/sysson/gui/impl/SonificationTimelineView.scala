/*
 *  SonificationTimelineView.scala
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

import at.iem.sysson.sound.Sonification
import de.sciss.lucre.bitemp.{SpanLike => SpanLikeEx}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm
import de.sciss.mellite.gui.TimelineObjView
import de.sciss.mellite.gui.TimelineObjView.Context
import de.sciss.mellite.gui.impl.timeline.TimelineObjViewImpl
import de.sciss.span.SpanLike
import de.sciss.synth.proc.FadeSpec

object SonificationTimelineView extends TimelineObjView.Factory {
  def typeID: Int = Sonification.typeID

  type E[S <: Sys[S]] = Sonification.Elem[S]

  TimelineObjView.addFactory(this)

  def apply[S <: Sys[S]](id: S#ID, span: Expr[S, SpanLike], obj: Sonification.Obj[S], context: Context[S])
                        (implicit tx: S#Tx): TimelineObjView[S] = {
    import SpanLikeEx._
    val res = new Impl(span = tx.newHandle(span), obj = tx.newHandle(obj))
    TimelineObjViewImpl.initAttrs    (span, obj, res)
    TimelineObjViewImpl.initGainAttrs(span, obj, res)
    TimelineObjViewImpl.initMuteAttrs(span, obj, res)
    TimelineObjViewImpl.initFadeAttrs(span, obj, res)
    res
  }

  private final class Impl[S <: Sys[S]](val span: stm.Source[S#Tx, Expr[S, SpanLike]],
                                        val obj : stm.Source[S#Tx, Sonification.Obj[S]])
    extends TimelineObjView[S]
    with TimelineObjView.HasMute
    with TimelineObjView.HasGain
    with TimelineObjView.HasFade { self =>

    override def toString = s"SonificationTimelineView($name, $spanValue)"

    var trackIndex  : Int             = _
    var trackHeight : Int             = _
    var nameOption  : Option[String]  = _
    var spanValue   : SpanLike        = _
    var gain        : Double          = _
    var muted       : Boolean         = _
    var fadeIn      : FadeSpec        = _
    var fadeOut     : FadeSpec        = _

    def dispose()(implicit tx: S#Tx) = ()
  }
}
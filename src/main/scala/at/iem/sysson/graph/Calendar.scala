/*
 *  Calendar.scala
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

package at.iem.sysson.graph

import de.sciss.synth
import de.sciss.synth.proc.{UGenGraphBuilder => UGB}
import de.sciss.synth.{AudioRated, UGenInLike}

object Calendar {
  sealed trait GE extends synth.GE.Lazy with UGB.Input with UGB.Key with AudioRated {
    def time: Dim.Play

    final type Key      = GE
    final type Value    = UGB.Unit

    final def key: Key  = this

    /** Similar to conventions of `java.text.SimpleDateFormat` */
    protected def timeBase: Char

    protected def modulo: Int

    private[sysson] def ctlName: String = {
      val dim = time.dim
      s"$$cal_dim_${dim.variable.name}_${dim.name}_$timeBase"
    }

    protected final def makeUGens: UGenInLike = {
      import synth._
      import Ops.stringToControl
      val b = UGB.get
      b.requestInput(this)

      val ctl = ctlName.ir(Vector(1f, 0f, 1f))    // mul1, add, mul2
      val mul1  = ctl \ 0
      val add   = ctl \ 1
      val mul2  = ctl \ 2
      val sig   = (time * mul1 + add) * mul2

      if (modulo == 0) sig else sig % modulo
    }
  }

  /** Year. This is an absolute fractional value, such as 1990.2,
    * which can be truncated using `.floor`.
    *
    * The precision is 0.5 days due to a simplification that
    * bases the year on 365.2422 days. It is positively biased
    * so that `.floor` should always give the correct year for
    * January 1st.
    */
  final case class Year(time: Dim.Play) extends GE {

    override def productPrefix  = s"Calendar$$Year"

    override def toString       = s"Calendar.Year($time)"

    protected def timeBase      = 'y'
    protected def modulo        = 0
  }

  /** Month in year. __Note:__ this counts from zero (January) to eleven (December).
    * This is a fractional value, such as 2.54,
    * which can be truncated using `.floor`.
    *
    * The precision is 0.5 days due to a simplification that
    * bases the year on 365.2422 days. It is positively biased
    * so that `.floor` should always give the correct month for
    * January 1st.
    */
  final case class Month(time: Dim.Play) extends GE {

    override def productPrefix  = s"Calendar$$Month"

    override def toString       = s"Calendar.Month($time)"

    protected def timeBase      = 'M'
    protected def modulo        = 12
  }
}

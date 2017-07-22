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

    protected def hasModulo: Boolean

    private[sysson] def ctlName: String = {
      val dim = time.dim
      s"$$cal_dim_${dim.variable.name}_${dim.name}_$timeBase"
    }

    protected final def makeUGens: UGenInLike = {
      import synth._
      import Ops.stringToControl
      val b = UGB.get
      b.requestInput(this)
      if (hasModulo) {
        val ctl = ctlName.ir(Vector(0f, 1f, 1f))    // add, mul, mod
        val add = ctl \ 0
        val mul = ctl \ 1
        val mod = ctl \ 2
        ((time + add) * mul) % mod

      } else {
        val ctl = ctlName.ir(Vector(0f, 1f))        // add, mul
        val add = ctl \ 0
        val mul = ctl \ 1
        (time + add) * mul
      }
    }
  }

  /** Year */
  final case class Year(time: Dim.Play) extends GE {

    override def productPrefix = s"Calendar$$Year"

    override def toString = s"Calendar.Year($time)"

    protected def timeBase  = 'y'
    protected def hasModulo = false
  }

  /** Month in year */
  final case class Month(time: Dim.Play) extends GE {

    override def productPrefix = s"Calendar$$Month"

    override def toString = s"Calendar.Month($time)"

    protected def timeBase  = 'M'
    protected def hasModulo = true
  }
}

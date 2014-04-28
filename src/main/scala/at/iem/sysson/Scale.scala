/*
 *  Scale.scala
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

object Scale {

  //  implicit def fromFunction(fun: Double => Double): Scale = Wrap(fun)

  case class LinLin(srcLo: Double, srcHi: Double, dstLo: Double, dstHi: Double, clip: Boolean = false)
    extends (Double => Double) {

    def apply(in: Double): Double = {
      val in1 = if (clip) math.max(dstLo, math.min(dstHi, in)) else in
      (in1 - srcLo) / (srcHi - srcLo) * (dstHi - dstLo) + dstLo
    }
  }

  case class LinExp(srcLo: Double, srcHi: Double, dstLo: Double, dstHi: Double, clip: Boolean = false)
    extends (Double => Double) {

    def apply(in: Double): Double = {
      val in1 = if (clip) math.max(dstLo, math.min(dstHi, in)) else in
      math.pow(dstHi / dstLo, (in1 - srcLo) / (srcHi - srcLo)) * dstLo
    }
  }

  final val Identity: Scale = Predef.identity

  //  case class Wrap(fun: Double => Double) extends Scale {
  //    def apply(in: Double) = fun(in)
  //  }
}
//trait Scale extends (Double => Double)
/*
 *  Scale.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
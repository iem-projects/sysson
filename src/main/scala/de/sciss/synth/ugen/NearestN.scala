/*
 *  NearestN.scala
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

package de.sciss.synth
package ugen

import collection.immutable.{IndexedSeq => Vec}

object NearestN {
  def kr(buf: GE, in: GE, gate: GE = 1f, num: Int = 1): NearestN = apply(control, buf, in, gate, num)
}
final case class NearestN(rate: Rate, buf: GE, in: GE, gate: GE, num: Int) extends UGenSource.MultiOut {
  protected def makeUGens: UGenInLike = unwrap(Vec(buf.expand, gate.expand, num: UGenIn).++(in.expand.outputs))

  protected def makeUGen(args: Vec[UGenIn]): UGenInLike =
    new UGen.MultiOut(name, rate, Vec.fill(num * 3)(rate), args)
}
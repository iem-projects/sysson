/*
 *  NearestN.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
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
    UGen.MultiOut(name, rate, Vec.fill(num * 3)(rate), args)
}
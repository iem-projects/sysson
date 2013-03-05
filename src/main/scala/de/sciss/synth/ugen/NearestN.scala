package de.sciss.synth
package ugen

import collection.immutable.{IndexedSeq => IIdxSeq}

object NearestN {
  def kr(buf: GE, in: GE, gate: GE = 1f, num: Int = 1): NearestN = apply(control, buf, in, gate, num)
}
final case class NearestN(rate: Rate, buf: GE, in: GE, gate: GE, num: Int) extends UGenSource.MultiOut {
  protected def makeUGens: UGenInLike = unwrap(Vector(buf.expand, gate.expand, num: UGenIn).++(in.expand.outputs))

  protected def makeUGen(args: IIdxSeq[UGenIn]): UGenInLike =
    new UGen.MultiOut(name, rate, Vector.fill(num * 3)(rate), args)
}
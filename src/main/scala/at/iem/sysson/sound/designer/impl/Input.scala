package at.iem.sysson.sound.designer.impl

import de.sciss.synth.{MaybeRate, UGenSpec}
import collection.immutable.{IndexedSeq => IIdxSeq}

private[impl] sealed trait Input
private[impl] final class GraphElem(val spec: UGenSpec, val rate: MaybeRate) extends Input {
  def name                                        = spec.name
  var inputs: IIdxSeq[Option[Input]]              = spec.args.map { a =>
    a.defaults.get(rate).map(v => ConstElem(Vector(v)))
  }
  var outputs: IIdxSeq[Option[(GraphElem, Int)]]  = IIdxSeq.fill(spec.outputs.size)(None)
  def numIns  = inputs.size
  def numOuts = outputs.size
}
private[impl] final case class ConstElem(values: IIdxSeq[UGenSpec.ArgumentValue]) extends Input


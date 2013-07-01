package at.iem.sysson
package sound
package designer
package impl

import de.sciss.synth.{MaybeRate, UGenSpec}

private[impl] sealed trait Input
private[impl] final class GraphElem(val spec: UGenSpec, val rate: MaybeRate) extends Input {
  def name                                        = spec.name
  var inputs: Vec[Option[Input]]              = spec.args.map { a =>
    a.defaults.get(rate).map(v => ConstElem(Vector(v)))
  }
  var outputs: Vec[Option[(GraphElem, Int)]]  = Vec.fill(spec.outputs.size)(None)
  def numIns  = inputs.size
  def numOuts = outputs.size
}
private[impl] final case class ConstElem(values: Vec[UGenSpec.ArgumentValue]) extends Input


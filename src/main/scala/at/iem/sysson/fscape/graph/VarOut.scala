package at.iem.sysson
package fscape.graph

import de.sciss.file.File
import de.sciss.fscape.UGen.Aux
import de.sciss.fscape.stream.{StreamIn, StreamOut}
import de.sciss.fscape.{GE, UGen, UGenGraph, UGenIn, UGenInLike, UGenSource, stream}

final case class VarOut(file: File, spec: Var.Spec, in: GE) extends UGenSource.SingleOut {
  protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = ??? // unwrap(in.expand.outputs)

  protected def makeUGen(args: Vec[UGenIn])(implicit b: UGenGraph.Builder): UGenInLike =
    UGen.SingleOut(this, inputs = args,
      aux = Aux.FileOut(file) :: ??? /* Aux.AudioFileSpec(spec) */ :: Nil, isIndividual = true, hasSideEffect = true)

  def makeStream(args: Vec[StreamIn])(implicit b: stream.Builder): StreamOut = {
    ??? // stream.AudioFileOut(file = file, spec = spec, in = args.map(_.toDouble))
  }
}

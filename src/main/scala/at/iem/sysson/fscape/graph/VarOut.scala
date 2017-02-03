package at.iem.sysson
package fscape.graph

import de.sciss.file.File
import de.sciss.fscape.UGen.Aux
import de.sciss.fscape.UGenSource._
import de.sciss.fscape.lucre.{UGenGraphBuilder => UGB}
import de.sciss.fscape.stream.{StreamIn, StreamOut}
import de.sciss.fscape.{GE, UGen, UGenGraph, UGenIn, UGenInLike, UGenSource, stream}

object VarOut {
  final case class WithRef(file: File, ref: Var.Spec.Value, in: GE) extends UGenSource.SingleOut {
    protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike =
      unwrap(this, outputs(expand(in)))

    protected def makeUGen(args: Vec[UGenIn])(implicit b: UGenGraph.Builder): UGenInLike =
      UGen.SingleOut(this, inputs = args,
        aux = Aux.FileOut(file) :: ref :: Nil, isIndividual = true, hasSideEffect = true)

    def makeStream(args: Vec[StreamIn])(implicit b: stream.Builder): StreamOut = {
      ??? // stream.AudioFileOut(file = file, spec = spec, in = args.map(_.toDouble))
    }
  }
}
final case class VarOut(key: String, spec: Var.Spec, in: GE) extends GE.Lazy {
  protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = {
    val ub = UGB.get(b)
    val f  = ub.requestInput(UGB.Input.Attribute(key)).peer.fold[File] {
      sys.error(s"Missing Attribute $key")
    } {
      case f: File  => f
      case other    => sys.error(s"$this - requires Artifact value, found $other")
    }
    val ref = ub.requestInput(spec)
    VarOut.WithRef(f, ref = ref, in = in)
  }
}

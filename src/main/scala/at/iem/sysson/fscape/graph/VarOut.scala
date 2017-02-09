/*
 *  VarOut.scala
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

package at.iem.sysson
package fscape.graph

import de.sciss.file.File
import de.sciss.fscape.UGen.Aux
import de.sciss.fscape.UGenSource._
import de.sciss.fscape.lucre.{UGenGraphBuilder => UGB}
import de.sciss.fscape.stream.{StreamIn, StreamOut}
import de.sciss.fscape.{GE, UGen, UGenGraph, UGenIn, UGenInLike, UGenSource, stream}

object VarOut {
  final case class WithRef(file: File, ref: Matrix.Spec.Value, in: GE) extends UGenSource.SingleOut {
    protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = {
      val out = outputs(expand(in))
      require(out.size == 1)
      unwrap(this, out)
    }

    protected def makeUGen(args: Vec[UGenIn])(implicit b: UGenGraph.Builder): UGenInLike =
      UGen.SingleOut(this, inputs = args,
        aux = Aux.FileOut(file) :: ref :: Nil, isIndividual = true, hasSideEffect = true)

    def makeStream(args: Vec[StreamIn])(implicit b: stream.Builder): StreamOut = {
      assert(args.size == 1)
      stream.VarOut(file = file, spec = ref, in = args.head.toDouble)
    }
  }
}
final case class VarOut(key: String, spec: Matrix.Spec, in: GE) extends GE.Lazy {
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

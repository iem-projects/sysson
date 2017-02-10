/*
 *  MkMatrix.scala
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

import de.sciss.file._
import de.sciss.fscape.UGen.Aux
import de.sciss.fscape.UGenSource.{outputs, unwrap, expand}
import de.sciss.fscape.lucre.FScape.Output
import de.sciss.fscape.lucre.UGenGraphBuilder
import de.sciss.fscape.lucre.UGenGraphBuilder.OutputRef
import de.sciss.fscape.stream.{StreamIn, StreamOut, Builder => SBuilder}
import de.sciss.fscape.{GE, UGen, UGenGraph, UGenIn, UGenInLike, UGenSource, stream}
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.matrix.{DataSource, Matrix => LMatrix}
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.serial.DataInput
import de.sciss.synth.proc.WorkspaceHandle

import scala.collection.immutable.{IndexedSeq => Vec}

object MkMatrix {
  final case class WithRef(spec: Matrix.Spec.Value, in: GE, ref: OutputRef) extends UGenSource.SingleOut {

    protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = {
      val out = outputs(expand(in))
      require(out.size == 1)
      unwrap(this, out)
    }

    protected def makeUGen(args: Vec[UGenIn])(implicit b: UGenGraph.Builder): UGenInLike =
      UGen.SingleOut(this, args, aux = Aux.String(ref.key) :: Nil, hasSideEffect = true)

    def makeStream(args: Vec[StreamIn])(implicit b: SBuilder): StreamOut = {
      assert(args.size == 1)
      val in    = args.head.toDouble
      val file  = ref.createCacheFile()
      stream.MkMatrix(file = file, spec = spec, in = in, ref = ref)
    }

    override def productPrefix: String = s"MkMatrix$$WithRef"
  }
}
/** A graph element that creates a UGen writing to an audio file
  * designated by an `FScape.Output` with a given `key` and the
  * value being an `AudioCue`.
  *
  * @param key          the key into the enclosing object's outputs map,
  *                     producing an `AudioCue`
  * @param in           the signal to write
  */
final case class MkMatrix(key: String, spec: Matrix.Spec, in: GE)
  extends GE.Lazy with Output.Reader {

  private def fail(arg: String, detail: String): Nothing =
    throw new IllegalArgumentException(s"$productPrefix.$arg cannot be resolved at initialization time: $detail")

  def tpe: Obj.Type = LMatrix

  def readOutput[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx, workspace: WorkspaceHandle[S]): Obj[S] = {
    val f   = file(in.readUTF())
    val loc = ArtifactLocation.newConst[S](f.parent)
    val art = Artifact(loc, Artifact.Child(f.name))
    import WorkspaceResolver.apply
    val ds  = DataSource(art)
    ds.variables.last
  }

  protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = {
    val ub          = UGenGraphBuilder.get(b)
    val refOpt      = ub.requestOutput(this)
    val specVal     = ub.requestInput(spec)
    val ref         = refOpt.getOrElse(sys.error(s"Missing output $key"))
    MkMatrix.WithRef(spec = specVal, in = in, ref = ref)
  }
}
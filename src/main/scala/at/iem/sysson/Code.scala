package at.iem.sysson

import de.sciss.serial.{Writable, DataInput, DataOutput, ImmutableSerializer}
import impl.{CodeImpl => Impl}
import scala.concurrent.Future
import de.sciss.synth
import scala.annotation.switch

// shamelessly ripped from Mellite...
object Code {
  final case class CompilationFailed() extends Exception
  final case class CodeIncomplete()    extends Exception

  implicit def serializer: ImmutableSerializer[Code] = Impl.serializer

  def read(in: DataInput): Code = serializer.read(in)

  def apply(id: Int, source: String): Code = (id: @switch) match {
    // case FileTransform.id => FileTransform(source)
    case SynthGraph   .id => SynthGraph   (source)
  }

  def future[A](fun: => A): Future[A] = Impl.future(fun)

  object SynthGraph {
    final val id    = 1
    final val name  = "Synth Graph"
  }
  final case class SynthGraph(source: String) extends Code {
    type In     = Unit
    type Out    = synth.SynthGraph
    def id      = SynthGraph.id

    def compileBody(): Future[Unit] = Impl.compileBody[In, Out, SynthGraph](this)

    def execute(in: In): Out = Impl.execute[In, Out, SynthGraph](this, in)

    def contextName = SynthGraph.name
  }
}
sealed trait Code extends Writable {
  /** The interfacing input type */
  type In
  /** The interfacing output type */
  type Out

  /** Identifier to distinguish types of code. */
  def id: Int

  /** Source code. */
  def source: String

  /** Human readable name. */
  def contextName: String

  /** Compiles the code body without executing it. */
  def compileBody(): Future[Unit]

  /** Compiles and executes the code. Returns the wrapped result. */
  def execute(in: In): Out // = compile()(in)

  def write(out: DataOutput): Unit = Code.serializer.write(this, out)
}
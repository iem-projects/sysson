/*
 *  CodeImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package impl

import scala.concurrent._
import java.io.File
import scala.tools.nsc
import scala.tools.nsc.interpreter.{Results, IMain}
import scala.tools.nsc.{ConsoleWriter, NewLinePrintWriter}
import de.sciss.processor.impl.ProcessorImpl
import collection.immutable.{Seq => ISeq}
import reflect.runtime.universe.{typeTag, TypeTag}
import scala.util.{Success, Failure}
import scala.concurrent.duration.Duration
import de.sciss.synth
import de.sciss.serial.{ImmutableSerializer, DataOutput, DataInput}
import java.util.concurrent.Executors

// shameless ripped from Mellite...
object CodeImpl {
  private final val COOKIE  = 0x436F6465  // "Code"
  implicit object serializer extends ImmutableSerializer[Code] {
    def write(v: Code, out: DataOutput): Unit = {
      out.writeInt(COOKIE)
      out.writeInt(v.id)
      out.writeUTF(v.source)
    }

    def read(in: DataInput): Code = {
      val cookie = in.readInt()
      require(cookie == COOKIE, s"Unexpected cookie $cookie (requires $COOKIE)")
      val id      = in.readInt()
      val source  = in.readUTF()
      Code.apply(id, source)
    }
  }

  // ---- internals ----

  // note: the Scala compiler is _not_ reentrant!!
  private implicit val executionContext = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  object Wrapper {
    implicit object SynthGraph
      extends Wrapper[Unit, synth.SynthGraph, Code.SynthGraph] {

      def imports: ISeq[String] = ISeq(
        "de.sciss.synth._",
        "de.sciss.synth.ugen._",
        "at.iem.sysson.graph._"
      )

      def binding = None

      def wrap(in: Unit)(fun: => Any): synth.SynthGraph = synth.SynthGraph(fun)

      def blockTag = typeTag[Unit]
    }
  }
  trait Wrapper[In, Out, Repr] {
    def imports: ISeq[String]
    def binding: Option[String]

    /** When `execute` is called, the result of executing the compiled code
      * is passed into this function.
      *
      * @param in   the code type's input
      * @param fun  the thunk that executes the coe
      * @return     the result of `fun` wrapped into type `Out`
      */
    def wrap(in: In)(fun: => Any): Out

    /** TypeTag of */
    def blockTag: TypeTag[_]
  }

  def execute[I, O, Repr <: Code { type In = I; type Out = O }](code: Repr, in: I)
                      (implicit w: Wrapper[I, O, Repr]): O = {
    w.wrap(in) {
      compileThunk(code.source, w, execute = true)
    }
  }

  def future[A](fun: => A): Future[A] = concurrent.future(fun)

  def compileBody[I, O, Repr <: Code { type In = I; type Out = O }](code: Repr)
                      (implicit w: Wrapper[I, O, Repr]): Future[Unit] = {
    future {
      blocking {
        compileThunk(code.source, w, execute = false)
      }
    }
  }

  private final class Intp(cset: nsc.Settings)
    extends IMain(cset, new NewLinePrintWriter(new ConsoleWriter, autoFlush = true)) {

    override protected def parentClassLoader = CodeImpl.getClass.getClassLoader
  }

  private lazy val intp = {
    val cset = new nsc.Settings()
    cset.classpath.value += File.pathSeparator + sys.props("java.class.path")
    val res = new Intp(cset)
    res.initializeSynchronous()
    res
  }

  object Run {
    def apply[A](execute: Boolean)(thunk: => A): A = if (execute) thunk else null.asInstanceOf[A]
  }

  sealed trait Context[A] {
    def __context__(): A
  }
  
  object FileTransformContext extends Context[FileTransformContext#Bindings] {
    private[CodeImpl] val contextVar = new ThreadLocal[FileTransformContext#Bindings]
    def __context__(): FileTransformContext#Bindings = contextVar.get()
  }

  final class FileTransformContext(in: File, out: File, fun: () => Any)
    extends ProcessorImpl[Unit, FileTransformContext] {
    process =>

    type Bindings = Bindings.type
    object Bindings {
      def in : File = process.in
      def out: File = process.out
      def checkAborted(): Unit = process.checkAborted()
      def progress(f: Double): Unit = {
        process.progress = f
        process.checkAborted()
      }
    }

    protected def body(): Unit = {
      // blocking {
      val prom  = Promise[Unit]()
      val t = new Thread {
        override def run(): Unit = {
          FileTransformContext.contextVar.set(Bindings)
          try {
            fun()
            prom.complete(Success(()))
          } catch {
            case e: Exception =>
              e.printStackTrace()
              prom.complete(Failure(e))
          }
        }
      }
      t.start()
      Await.result(prom.future, Duration.Inf)
      // }
    }
  }

  private val pkg = "at.iem.sysson.impl.CodeImpl"

  // note: synchronous
  private def compileThunk(code: String, w: Wrapper[_, _, _], execute: Boolean): Any = {
    val i = intp

    val impS  = w.imports.map(i => s"  import $i\n").mkString
    val bindS = w.binding.map(i =>
     s"""  val __context__ = $pkg.$i.__context__
        |  import __context__._
      """.stripMargin
    ).getOrElse("")
    val aTpe  = w.blockTag.tpe.toString
    val synth =
     s"""$pkg.Run[$aTpe]($execute) {
        |$impS
        |$bindS
        |
        |""".stripMargin + code + "\n}"

    val res = i.interpret(synth)
    // commented out to chase ClassNotFoundException
    // i.reset()
    res match {
      case Results.Success =>
        if (aTpe == "Unit" || !execute) () else {
          val n = i.mostRecentVar
          i.valueOfTerm(n).getOrElse(sys.error(s"No value for term $n"))
        }

      case Results.Error      => throw Code.CompilationFailed()
      case Results.Incomplete => throw Code.CodeIncomplete()
    }
  }
}
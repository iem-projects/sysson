package at.iem.sysson.util

import de.sciss.synth.proc
import de.sciss.synth.proc.SoundProcesses
import de.sciss.synth.proc.impl.{CodeImpl, MemoryClassLoader}

import scala.concurrent.stm.{Ref, atomic}
import scala.concurrent.{Future, Promise, blocking}

object DoubleTransform {
  private val count = Ref(0)

  def init(): Unit = Code.init()

  def compile(source: Code)(implicit compiler: proc.Code.Compiler): Future[DoubleTransform] = {
    val p       = Promise[DoubleTransform]()
    val idx     = count.single.transformAndGet(_ + 1)
    val name    = s"DoubleTransform$idx"
    performCompile(p, name, source)
    p.future
  }

  private lazy val classLoader = new MemoryClassLoader

  private def performCompile(p: Promise[DoubleTransform], name: String, source: Code)
                            (implicit compiler: proc.Code.Compiler): Unit = {
    val jarFut: Future[Array[Byte]] = proc.Code.future(blocking(source.execute(name)))
    import SoundProcesses.executionContext
    val tFut: Future[DoubleTransform] = jarFut.map { jar =>
      val cl = classLoader
      atomic { implicit tx =>
        cl.add(name, jar)
      }
      val fullName  = s"${proc.Code.UserPackage}.$name"
      val clazz     = Class.forName(fullName, true, cl)
      //  println("Instantiating...")
      val fun = clazz.newInstance().asInstanceOf[Double => Double]
      new Impl(fun)
    } (executionContext)
    p.completeWith(tFut)
  }

  private final class Impl(val peer: Double => Double) extends DoubleTransform

  object Code extends proc.Code.Type {
    final val id      = 0x30000
    final val prefix  = "Double => Double"
    type      Repr    = Code

    def humanName: String = prefix

    def docBaseSymbol: String = "de.sciss"

    private[this] lazy val _init: Unit = {
      proc.Code.addType(this)
      proc.Code.registerImports(id, Vector(
        "de.sciss.numbers.Implicits._"
      ))
    }

    // override because we need to register imports
    override def init(): Unit = _init

    def mkCode(source: String): Repr = Code(source)
  }

  /** The source code may access the input argument
    * via symbol `x`. It must return a `Double`.
    */
  final case class Code(source: String) extends proc.Code {
    type In     = String
    type Out    = Array[Byte]

    def tpe: proc.Code.Type = Code

    def compileBody()(implicit compiler: proc.Code.Compiler): Future[Unit] =
      proc.Code.future(blocking { execute("Unnamed"); () })

    def execute(in: In)(implicit compiler: proc.Code.Compiler): Out =
      CodeImpl.compileToJar(in, this, prelude = mkPrelude(in), postlude = postlude)

    private def mkPrelude(name: String): String = {
      s"""final class $name extends Function1[Double, Double] {
         |  def apply(x: Double): Double = {
         |""".stripMargin
    }

    def prelude: String = mkPrelude("Main")

    def postlude: String = "\n  }\n}\n"

    def updateSource(newText: String): Code = copy(source = newText)
  }
}
trait DoubleTransform {
  def peer: Double => Double
}
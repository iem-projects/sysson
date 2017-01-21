package at.iem.sysson

import de.sciss.file._
import de.sciss.filecache.Limit
import de.sciss.fscape.Graph
import de.sciss.fscape.lucre.FScape.Output
import de.sciss.fscape.lucre.{Cache, FScape}
import de.sciss.lucre.expr.DoubleObj
import de.sciss.lucre.synth.InMemory
import de.sciss.synth.proc.{GenContext, GenView, WorkspaceHandle}

import scala.concurrent.stm.Ref
import scala.util.{Failure, Success}

object FScapeTest extends App {
  implicit val cursor = InMemory()
  type S              = InMemory

  FScape.init()
  GenView.addFactory(FScape.genViewFactory())
  val folder = userHome / "Documents" / "temp" / "fscape_test"
  folder.mkdir()
  Cache.init(folder = folder, capacity = Limit())

  cursor.step { implicit tx =>
    val f = FScape[S]
    val g = Graph {
      import at.iem.sysson.fscape.graph._
      import de.sciss.fscape._
      import de.sciss.fscape.graph._
      import de.sciss.fscape.lucre.graph._
      1.poll(0, label = "rendering")
      val v     = Var("var")
      val value = v.playLinear()
      val mx    = RunningMax(value).last
      MkDouble("out", mx)
    }
    val out = f.outputs.add("out", DoubleObj)
    f.graph() = g

    val count = Ref(0)

    import WorkspaceHandle.Implicits.dummy
    implicit val genCtx = GenContext[S]

    def mkView(out: Output[S]): GenView[S] = {
      val view = GenView(out)

      import de.sciss.lucre.stm.TxnLike.peer
      view.reactNow { implicit tx => upd =>
        if (upd.isComplete) {
          view.value.foreach { value =>
            value match {
              case Success(v)  =>
                println(s"Value is now $v")
              case Failure(ex) =>
                println("Value failed:")
                ex.printStackTrace()
            }
            if (count.transformAndGet(_ + 1) == 2) tx.afterCommit(sys.exit())
          }
        }
      }
      view
    }

    val view1 = mkView(out)

    new Thread {
      override def run(): Unit = Thread.sleep(Long.MaxValue)
      start()
    }
  }
}
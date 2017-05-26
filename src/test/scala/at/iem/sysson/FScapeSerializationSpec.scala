package at.iem.sysson

import de.sciss.fscape.Graph
import de.sciss.fscape.lucre.FScape
import de.sciss.lucre.expr.DoubleObj
import de.sciss.lucre.stm.Durable
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.synth.proc.SoundProcesses
import org.scalatest.{Matchers, Outcome, fixture}

class FScapeSerializationSpec extends fixture.FlatSpec with Matchers {
  type S = Durable
  type FixtureParam = S

  SoundProcesses.init()
  FScape        .init()

  protected def withFixture(test: OneArgTest): Outcome = {
    val store  = BerkeleyDB.tmp()
    val system = Durable(store)
    try {
      test(system)
    } finally {
      system.close()
    }
  }

  "An FScape object" should "be serializable" in { cursor =>
    val (fH, numSources) = cursor.step { implicit tx =>
      val f = FScape[S]
      val g = Graph {
        import at.iem.sysson.fscape.graph._
        import de.sciss.fscape._
        import de.sciss.fscape.graph._
        import lucre.graph._
        1.poll(0, label = "rendering")
        val v   = Matrix("var")
        val vp  = v.valueSeq
        val mn  = RunningMin(vp).last
        val mx  = RunningMax(vp).last
        MkDouble("min", mn)
        MkDouble("max", mx)
      }
      /* val out1 = */ f.outputs.add("min", DoubleObj)
      /* val out2 = */ f.outputs.add("max", DoubleObj)
      f.graph() = g
      tx.newHandle(f) -> g.sources.size
    }

    cursor.step { implicit tx =>
      val f = fH()
      val g = f.graph.value
      assert(g.sources.size === numSources)
      val outputs = f.outputs.iterator.toList.sortBy(_.key)
      assert(outputs.size === 2)
      val out1 :: out2 :: Nil = outputs
      assert(out1.key       === "max")
      assert(out1.valueType === DoubleObj)
      assert(out2.key       === "min")
      assert(out2.valueType === DoubleObj)
    }
  }
}
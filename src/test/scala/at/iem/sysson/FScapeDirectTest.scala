package at.iem.sysson

import at.iem.sysson.fscape.GenViewFactory
import de.sciss.file._
import de.sciss.filecache.Limit
import de.sciss.fscape.Graph
import de.sciss.fscape.lucre.FScape.Output
import de.sciss.fscape.lucre.{Cache, FScape}
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.DoubleObj
import de.sciss.lucre.matrix.DataSource
import de.sciss.lucre.synth.InMemory
import de.sciss.synth.proc.{GenContext, GenView, WorkspaceHandle}

import scala.concurrent.stm.Ref
import scala.util.{Failure, Success}

// refer to a matrix directly in the FScape object's attribute map
object FScapeDirectTest extends App {
  implicit val cursor = InMemory()
  type S              = InMemory

  FScape.init()
  GenViewFactory.install()
  val folder = userHome / "Documents" / "temp" / "fscape_test"
  folder.mkdir()
  Cache.init(folder = folder, capacity = Limit())

  val dir   = userHome / "sysson" / "nc"
  val mName = "5x30-climatology_2001-05-01_2016-05-01_RO_OPSv5.6.2_L2b_no_METOP_no_TerraSAR-X.nc"
  val inF   = dir / mName
  require(inF.isFile)
  val vName = "Temperature"

  import WorkspaceHandle.Implicits.dummy
  implicit val resolver: DataSource.Resolver[S] = WorkspaceResolver[S]

  cursor.step { implicit tx =>
    val loc = ArtifactLocation.newConst[S](dir)
    val art = Artifact(loc, Artifact.Child(mName))
    val ds  = DataSource(art)
    val mat = ds.variables.find(_.name == vName).getOrElse(sys.error(s"No variable '$vName' in nc file"))

    val f   = FScape[S]
    val g = Graph {
      import at.iem.sysson.fscape.graph._
      import de.sciss.fscape._
      import de.sciss.fscape.graph._
      import de.sciss.fscape.lucre.graph._
      1.poll(0, label = "rendering")
      val v     = Matrix("var")
      val v0    = v.valueSeq
      val value = Gate(v0, v0 < 1000 & v0 > -1000)   // cheesy way drop NaNs. By the way, we need a Drop UGen!
      val mx    = RunningMax(value).last
      val mn    = RunningMin(value).last
      MkDouble("max", mx)
      MkDouble("min", mn)
    }
    val outMx = f.outputs.add("max", DoubleObj)
    val outMn = f.outputs.add("min", DoubleObj)
    f.graph() = g
    f.attr.put("var", mat)

    val count = Ref(0)

    implicit val genCtx = GenContext[S]

    def mkView(out: Output[S], total: Int): GenView[S] = {
      val view  = GenView(out)
      val key   = out.key

      import de.sciss.lucre.stm.TxnLike.peer
      view.reactNow { implicit tx => upd =>
        if (upd.isComplete) {
          view.value.foreach { value =>
            value match {
              case Success(v)  =>
                println(s"Value for $key is now $v")
              case Failure(ex) =>
                println(s"Value for $key failed:")
                ex.printStackTrace()
            }
            if (count.transformAndGet(_ + 1) == total) tx.afterCommit(sys.exit())
          }
        }
      }
      view
    }

    mkView(outMx, 2)
    mkView(outMn, 2)
  }

  new Thread {
    override def run(): Unit = Thread.sleep(Long.MaxValue)
    start()
  }
}
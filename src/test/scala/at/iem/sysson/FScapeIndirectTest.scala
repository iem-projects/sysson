package at.iem.sysson

import at.iem.sysson.sound.Sonification
import de.sciss.file._
import de.sciss.filecache.Limit
import de.sciss.fscape.Graph
import de.sciss.fscape.lucre.{Cache, FScape}
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.DoubleObj
import de.sciss.lucre.matrix.DataSource
import de.sciss.lucre.synth.InMemory
import de.sciss.synth.SynthGraph
import de.sciss.synth.proc.{AuralSystem, Transport, WorkspaceHandle}

// refer to a matrix indirectly in FScape, requiring look up in sonification
object FScapeIndirectTest extends App {
  implicit val cursor = InMemory()
  type S              = InMemory

  initTypes()
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

  val as = AuralSystem()

  def run()(implicit tx: S#Tx): Unit = {
    val loc = ArtifactLocation.newConst[S](dir)
    val art = Artifact(loc, Artifact.Child(mName))
    val ds  = DataSource(art)
    val mat = ds.variables.find(_.name == vName).getOrElse(sys.error(s"No variable '$vName' in nc file"))

    val f   = FScape[S]
    val gF  = Graph {
      import at.iem.sysson.fscape.graph._
      import de.sciss.fscape._
      import de.sciss.fscape.graph._
      import de.sciss.fscape.lucre.graph._
      1.poll(0, label = "rendering")
      val v     = Matrix("var")
      val v0    = v.valueSeq
      val value = Gate(v0, v0 < 1000 & v0 > -1000)   // cheesy way drop NaNs. By the way, we need a Drop UGen!
      val mn    = RunningMin(value).last
      val mx    = RunningMax(value).last
      MkDouble("min", mn)
      MkDouble("max", mx)
    }
    val outMn = f.outputs.add("min", DoubleObj)
    val outMx = f.outputs.add("max", DoubleObj)
    f.graph() = gF

    val son     = Sonification[S]
    val sources = son.sources.modifiableOption.get
    val source  = Sonification.Source(mat)
    sources.put("var", source)

    val gP      = SynthGraph {
      import de.sciss.synth.proc.graph.Ops._
      val mn  = "min".kr
      val mx  = "max".kr
      mn.poll(0, "min (RT)")
      mx.poll(0, "max (RT)")
    }

    son.proc.graph() = gP
    son.proc.attr.put("min", outMn)
    son.proc.attr.put("max", outMx)

    val t = Transport[S](as)
    t.addObject(son)
    t.play()
  }

  cursor.step { implicit tx =>
    as.whenStarted { s =>
      cursor.step { implicit tx =>
        println("Run.")
        run()
      }
    }
    as.start()
  }

  new Thread {
    override def run(): Unit = Thread.sleep(Long.MaxValue)
    start()
  }
}
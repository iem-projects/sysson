package at.iem.sysson

import at.iem.sysson.sound.Sonification
import de.sciss.file._
import de.sciss.filecache.Limit
import de.sciss.fscape.Graph
import de.sciss.fscape.lucre.{Cache, FScape}
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.matrix.DataSource
import de.sciss.lucre.synth.InMemory
import de.sciss.synth.proc.{GenContext, WorkspaceHandle}

// transform an input matrix by
// averaging over a specified dimension
object FScapeScenario extends App {
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
  val vName = "Temperature" // [Time:180][Longitude:12][Latitude:36][Altitude:601]
  val dName = "Longitude"

  import WorkspaceHandle.Implicits.dummy
  implicit val resolver: DataSource.Resolver[S] = WorkspaceResolver[S]

  cursor.step { implicit tx =>
    val locIn = ArtifactLocation.newConst[S](dir)
    val artIn = Artifact(locIn, Artifact.Child(mName))
    val ds    = DataSource(artIn)
    val mat   = ds.variables.find(_.name == vName).getOrElse(sys.error(s"No variable '$vName' in nc file"))

    val f   = FScape[S]
    val g = Graph {
      import at.iem.sysson.fscape.graph._
      import de.sciss.fscape._
      import de.sciss.fscape.graph._
      1.poll(0, label = "rendering")
      val v       = Var("var")
      val d       = Dim(v, dName /* "dim" */)
      val p       = v.playLinear()
      val dSz     = d.size
      val tSz     = d.succSize  // good name?
      val cSz     = dSz * tSz
      val m       = Metro(cSz)
      val sum     = RunningWindowSum(p, tSz, m)
      val last    = ResizeWindow(sum, size = cSz, start = cSz - dSz)
      val specIn  = v.spec
      val specOut = specIn.drop(d)
      VarOut("file", specOut, in = last)
    }
    f.graph() = g

    val locOut  = ArtifactLocation.newConst[S](userHome / "Documents" / "temp")
    val artOut  = Artifact(locIn, Artifact.Child("avg.nc"))

    f.attr.put("var" , mat)
    f.attr.put("file", artOut)

    implicit val genCtx = GenContext[S]

    val r = Sonification.render[S](f)
    r.reactNow { implicit tx => state => if (state.isComplete) {
      val res = r.result.get
       println(s"Result: $res")
       sys.exit(if (res.isSuccess) 0 else 1)
    }}

    new Thread {
      override def run(): Unit = Thread.sleep(Long.MaxValue)
      start()
    }
  }
}
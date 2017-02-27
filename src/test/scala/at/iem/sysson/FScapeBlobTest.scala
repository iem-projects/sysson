package at.iem.sysson

import at.iem.sysson.fscape.GenViewFactory
import at.iem.sysson.sound.Sonification
import de.sciss.file._
import de.sciss.filecache.Limit
import de.sciss.fscape.Graph
import de.sciss.fscape.lucre.{Cache, FScape}
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.matrix.DataSource
import de.sciss.lucre.synth.InMemory
import de.sciss.synth.proc.{GenContext, WorkspaceHandle}

import scala.util.{Failure, Success}

object FScapeBlobTest extends App {
  implicit val cursor = InMemory()
  type S              = InMemory

  FScape.init()
  GenViewFactory.install()
  val folder = userHome / "Documents" / "temp" / "fscape_test"
  folder.mkdir()
  Cache.init(folder = folder, capacity = Limit())

  val dirIn   = userHome / "sysson" / "nc"
  val mName   = "5x30-climatology_2001-05-01_2016-05-01_ta_anom.nc"
  val inF     = dirIn / mName
  require(inF.isFile)
  val vName   = "Temperature"
  val dirOut  = userHome / "Documents" / "temp" / "fscape_test"
  dirOut.mkdirs()

  import WorkspaceHandle.Implicits.dummy
  implicit val resolver: DataSource.Resolver[S] = WorkspaceResolver[S]

  cursor.step { implicit tx =>
    val locIn = ArtifactLocation.newConst[S](dirIn)
    val artIn = Artifact(locIn, Artifact.Child(mName))
    val ds    = DataSource(artIn)
    val mat   = ds.variables.find(_.name == vName).getOrElse(sys.error(s"No variable '$vName' in nc file"))

    val f   = FScape[S]
    val g = Graph {
      import at.iem.sysson.fscape.graph._
      import de.sciss.fscape._
      import de.sciss.fscape.graph._
      1.poll(0, label = "rendering")
      val mIn       = Matrix("var")
      val d1        = Dim(mIn, "Time")
      val d2        = Dim(mIn, "Altitude")
      val voices    = 4
      val blobSz    = voices * 10
      val specIn    = mIn.spec
      val d3        = Dim.Def("blob", values = ArithmSeq(length = blobSz))
      val s1        = specIn.moveLast(d1)
      val s2        = s1    .drop    (d2)
      val specOut   = s2    .append  (d3)
      val win0      = mIn.valueWindow(d1, d2)  // row-dim, col-dim
      val taLo      = 0.0
      val taHi      = 3.5
      val win1      = win0.max(taLo).min(taHi) / taHi
      val win       = Gate(win1, !win1.isNaN)
      win.poll(win.isNaN, "NaN!")
      val width     = d2.size
      val height    = d1.size
      val winSize   = width * height
      width .poll(0, "width")
      height.poll(0, "height")
      val blobs     = Blobs2D(in = win, width = width, height = height, thresh = 0.265)
      val minWidth  = 10.0 / width    // XXX TODO --- make user selectable
      val minHeight =  3.0 / height   // XXX TODO --- make user selectable
      val el        = (winSize / ControlBlockSize()).ceil
      val mOut      = BlobVoices(in = win.elastic(el), width = width, height = height, blobs = blobs,
        minWidth = minWidth, minHeight = minHeight, voices = voices)

      MatrixOut("out", specOut, mOut)
    }
    f.graph() = g

    val locOut  = ArtifactLocation.newConst[S](dirOut)
    val artOut  = Artifact(locOut, Artifact.Child("blobs.nc"))

    f.attr.put("var", mat)
    f.attr.put("out", artOut)

    implicit val genCtx = GenContext[S]

    val r = Sonification.render[S](f)
    r.reactNow { implicit tx => state => if (state.isComplete) {
      r.result.get match {
        case Success(_) =>
          println("Success.")
          sys.exit(1)

        case Failure(ex) =>
          println("Failure:")
          ex.printStackTrace()
          sys.exit(1)
      }
    }}

    new Thread {
      override def run(): Unit = Thread.sleep(Long.MaxValue)
      start()
    }
  }
}
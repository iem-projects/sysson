package at.iem.sysson

import at.iem.sysson.fscape.GenViewFactory
import at.iem.sysson.sound.Sonification
import de.sciss.file._
import de.sciss.filecache.Limit
import de.sciss.fscape.Graph
import de.sciss.fscape.lucre.{Cache, FScape}
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.matrix.{DataSource, Dimension, Reduce}
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
//  val mName   = "5x30-climatology_2001-05-01_2016-05-01_ta_anom.nc"
  val mName   = "5x30-climatology_2001-05-01_2016-05-01_ta_anom2.nc"
  val inF     = dirIn / mName
  require(inF.isFile)
  val vName   = "Temperature"
  val dirOut  = userHome / "Documents" / "temp" / "fscape_test"
  dirOut.mkdirs()

  val altRange  = 210 to 360 // 390

  import WorkspaceHandle.Implicits.dummy
  implicit val resolver: DataSource.Resolver[S] = WorkspaceResolver[S]

  cursor.step { implicit tx =>
    val locIn = ArtifactLocation.newConst[S](dirIn)
    val artIn = Artifact(locIn, Artifact.Child(mName))
    val ds    = DataSource(artIn)
    val mat   = ds.variables.find(_.name == vName).getOrElse(sys.error(s"No variable '$vName' in nc file"))
    val red   = Reduce(mat, Dimension.Selection.Name[S]("Altitude"), Reduce.Op.Slice[S](altRange.head, altRange.last))

    val f   = FScape[S]
    val g = Graph {
      import at.iem.sysson.fscape.graph._
      import de.sciss.fscape._
      import de.sciss.fscape.graph._

      def printRange(in: GE, start: Int, stop: Int, label: String): Unit = {
        val slice = in.drop(start).take(stop - start)
        (slice zip slice).poll(Metro(2), label)
      }

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

      val width     = d2.size
      val height    = d1.size
      val winSzIn   = width * height

      val taLo      = 0.0
      val taHi      = 3.5
      val win1      = win0.max(taLo).min(taHi) / taHi
      val win       = Gate(win1, !win1.isNaN)

      val thresh    = 0.4 // 0.14 // 0.21 // 0.26
      val winEl     = BufferDisk(win)
      val blobs     = Blobs2D(in = win, width = width, height = height, thresh = thresh, pad = 1)
      val minWidth  = 10.0 // XXX TODO --- make user selectable
      val minHeight =  4.0 // XXX TODO --- make user selectable

      val numBlobs    = blobs.numBlobs
      val mOut        = BlobVoices(in = winEl, width = width, height = height,
        numBlobs = numBlobs, bounds = blobs.bounds, numVertices = blobs.numVertices, vertices = blobs.vertices,
        minWidth = minWidth, minHeight = minHeight, voices = voices)
      val winSzOut  = blobSz * height

      val frames = MatrixOut("out", specOut, mOut)
      frames.poll(Metro(winSzOut).tail, "advance")

      RunningSum(numBlobs).last.poll(0, "total-blobs")
    }
    f.graph() = g

    val locOut  = ArtifactLocation.newConst[S](dirOut)
    val artOut  = Artifact(locOut, Artifact.Child("blobs14.nc"))

    f.attr.put("var", red   )
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
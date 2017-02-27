package at.iem.sysson

import at.iem.sysson.fscape.GenViewFactory
import de.sciss.file._
import de.sciss.fscape.lucre.FScape
import de.sciss.fscape.{GE, Graph, stream}
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.matrix.DataSource
import de.sciss.lucre.synth.InMemory
import de.sciss.synth.proc.{GenContext, WorkspaceHandle}

object FScapeSimpleBlobTest extends App {
  implicit val cursor = InMemory()
  type S              = InMemory

  FScape.init()
  GenViewFactory.install()
  val folder = userHome / "Documents" / "temp" / "fscape_test"
  folder.mkdir()

  val fIn     = userHome / "Documents" / "temp" / "blob_input.jpg"
  val fOut    = folder / "blob_out.nc"
  val width   = 633
  val height  = 526

  cursor.step { implicit tx =>
    val loc = ArtifactLocation.newConst[S](fOut.parent)
    val art = Artifact(loc, Artifact.Child(fOut.name))

    val f   = FScape[S]
    import WorkspaceHandle.Implicits.dummy
    implicit val resolver: DataSource.Resolver[S] = WorkspaceResolver[S]

    val g = Graph {
      import at.iem.sysson.fscape.graph._
      import de.sciss.fscape.graph._
      def mkImageIn() = ImageFileIn(file = fIn, numChannels = 1)

      val in      = mkImageIn()
      val blobs   = Blobs2D   (in = in , width = width, height = height, thresh = 0.3)
      val inE     = mkImageIn()
      val voices  = 4
      val blobVc  = BlobVoices(in = inE, width = width, height = height, blobs = blobs, voices = voices)

      def printAll(sig: GE, label: String): Unit = {
        val dup = sig zip sig // ResizeWindow(sig, 1, 0, 1)
        dup.poll(Metro(2), label)
      }

      def printOne(sig: GE, label: String): Unit = {
        sig.poll(0, label)
      }

      val dimTime = Matrix.Op.Append(Dim.Def("time", values = ArithmSeq(length = height)))
      val dimBlob = Matrix.Op.Append(Dim.Def("blob", values = ArithmSeq(length = voices * 10)))
      val spec    = Matrix.Spec(null, Vector(dimTime, dimBlob))
      MatrixOut("out", spec, in = blobVc)

      Length(blobVc).poll(0, "done")

  //    printAll(blobs.numBlobs   , "num-blobs   ")
  //    printAll(blobs.bounds     , "bounds      ")
  //    printAll(blobs.numVertices, "num-vertices")
  //    printAll(blobs.vertices   , "vertices    ")
    }

    f.graph() = g
    f.attr.put("out", art)

    implicit val genCtx = GenContext[S]

    val config = stream.Control.Config()
    config.useAsync = false // for debugging
//    val rendering = f.run(config)
    val rendering = GenViewFactory.render(f, config)

    rendering.reactNow { implicit tx => {
      case FScape.Rendering.Completed =>
        val Some(res) = rendering.result
        tx.afterCommit {
          println(s"DONE: $res")
          sys.exit(if (res.isFailure) 1 else 0)
        }
      case _ =>
    }}
  }

//  val ctrl = stream.Control(config)
//  ctrl.run(g)
//
//  Swing.onEDT {
//    SimpleGUI(ctrl)
//  }

  println("Running.")
}
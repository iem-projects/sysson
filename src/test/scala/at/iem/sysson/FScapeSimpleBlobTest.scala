package at.iem.sysson

import de.sciss.file._
import de.sciss.fscape.gui.SimpleGUI
import de.sciss.fscape.{GE, Graph, stream}

import scala.swing.Swing

object FScapeSimpleBlobTest extends App {
  val fIn     = userHome / "Documents" / "temp" / "blob_input.jpg"
  val width   = 633
  val height  = 526

  val g = Graph {
    import de.sciss.fscape.graph._
    import at.iem.sysson.fscape.graph._
    def mkImageIn() = ImageFileIn(file = fIn, numChannels = 1)

    val in      = mkImageIn()
    val blobs   = Blobs2D   (in = in , width = width, height = height, thresh = 0.3)
    val inE     = mkImageIn()
    val voices  = BlobVoices(in = inE, width = width, height = height, blobs = blobs)

    def printAll(sig: GE, label: String): Unit = {
      val dup = sig zip sig // ResizeWindow(sig, 1, 0, 1)
      dup.poll(Metro(2), label)
    }

    def printOne(sig: GE, label: String): Unit = {
      sig.poll(0, label)
    }

    Length(voices).poll(0, "done")

//    printAll(blobs.numBlobs   , "num-blobs   ")
//    printAll(blobs.bounds     , "bounds      ")
//    printAll(blobs.numVertices, "num-vertices")
//    printAll(blobs.vertices   , "vertices    ")
  }

  val config = stream.Control.Config()
  config.useAsync = false // for debugging
  val ctrl = stream.Control(config)

  ctrl.run(g)

  Swing.onEDT {
    SimpleGUI(ctrl)
  }

  println("Running.")
}
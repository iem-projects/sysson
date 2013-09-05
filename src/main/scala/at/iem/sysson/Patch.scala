package at.iem.sysson

import de.sciss.synth.SynthGraph
import at.iem.sysson.gui.DragAndDrop

object Patch {
  val empty = Patch("<empty>", SynthGraph {})
}
case class Patch(name: String, graph: SynthGraph)

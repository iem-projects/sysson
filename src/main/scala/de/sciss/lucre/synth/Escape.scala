package de.sciss.lucre.synth

import de.sciss.lucre.synth.{Server => LServer, SynthDef => LSynthDef}
import de.sciss.synth.SynthGraph

object Escape {
  def getSynthDef(server: LServer, graph: SynthGraph, nameHint: Option[String])(implicit tx: Txn): LSynthDef =
    NodeGraph.getSynthDef(server, graph, nameHint)

  //  def addMessage(resource: Resource, m: osc.Message with message.Send, audible: Boolean,
  //                 dependencies: List[Resource] = Nil)(implicit tx: Txn): Unit =
  //    tx.addMessage(resource, m, dependencies = dependencies)
}

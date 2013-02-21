package at.iem.sysson
package sound
package impl

import de.sciss.synth
import synth._
import synth.impl.BasicUGenGraphBuilder
import collection.immutable.{IndexedSeq => IIdxSeq}
import ugen.ControlProxyLike

private[impl] object UGenGraphBuilderImpl {
   def apply(sonif: Sonification): UGenGraphBuilder = new Impl(sonif)

  private final class Impl(sonif: Sonification) extends BasicUGenGraphBuilder with UGenGraphBuilder {
    builder =>

    override def toString = s"UGenGraphBuilder(${sonif.name})@" + hashCode.toHexString

    var remaining:      IIdxSeq[Lazy]             = sonif.graph.sources
    var controlProxies: Set[ControlProxyLike[_]]  = sonif.graph.controlProxies
    var scanIns                                   = Map.empty[String, Int]

    def addMatrixIn(key: String): Int = {
      ???
//      val res = aural.scanInNumChannels(timed, time, key)(tx)
//      scanIns += key -> res
//      res
    }

    def build() {
      UGenGraph.use(this) {
        var g = sonif.graph
        var controlProxies = Set.empty[ControlProxyLike[_]]
        while (g.nonEmpty) {
          controlProxies ++= g.controlProxies
          g = SynthGraph(g.sources.foreach(force(_)))
        }
        build(controlProxies)
      }
    }
  }
}
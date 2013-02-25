package at.iem.sysson
package sound
package impl

import de.sciss.synth
import synth.impl.BasicUGenGraphBuilder
import synth._
import ugen.ControlProxyLike

private[impl] object UGenGraphBuilderImpl {
   def apply(sonif: Sonification, sg: SynthGraph): (UGenGraph, Set[String]) = new Impl(sonif).build(sg)

  private final class Impl(sonif: Sonification) extends BasicUGenGraphBuilder with UGenGraphBuilder {

    override def toString = s"UGenGraphBuilder(${sonif.name})@" + hashCode.toHexString

    private def controlName(key: String): String = "$son_" + key

    private var usedMappings = Set.empty[String]

    def addMatrixIn(m: MatrixIn): GE = {
      import ugen._
      val key       = m.key
      val source    = sonif.mapping.getOrElse(key, throw Sonification.MissingInput(key))
      val ctlName   = controlName(key)
      usedMappings += key
      source match {
        case col @ ColumnSource(_) =>
          val sig = BufRd.kr(numChannels = col.size, buf = ctlName.ir, index = 0, loop = 1, interp = 0)
          Latch(m.rate, sig)  // instant k- to a-rate

        case row @ RowSource(_) =>
            ???
      }
    }

    def build(init: SynthGraph): (UGenGraph, Set[String]) = {
      val ug = UGenGraph.use(this) {
        var g = init // sonif.graph
        var controlProxies = Set.empty[ControlProxyLike[_]]
        while (g.nonEmpty) {
          controlProxies ++= g.controlProxies
          g = SynthGraph(g.sources.foreach(force(_)))
        }
        build(controlProxies)
      }
      (ug, usedMappings)
    }
  }
}
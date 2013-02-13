package at.iem.sysson.sound.designer.impl

import java.awt.geom.Rectangle2D

private[impl] object Port {
  final case class In(idx: Int) extends Port {
    def visualRect(ports: VisualPorts)  = ports.inlets(idx)
    def name(elem: GraphElem)           = elem.spec.args(idx).name
  }
  final case class Out(idx: Int) extends Port {
    def visualRect(ports: VisualPorts)  = ports.outlets(idx)
    def name(elem: GraphElem)           = elem.spec.outputs(idx).name.getOrElse("out")
  }
}
private[impl] sealed trait Port {
  def visualRect(ports: VisualPorts): Rectangle2D
  def name(elem: GraphElem): String
}


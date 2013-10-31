package at.iem.sysson.gui
package impl

import GUI.requireEDT

trait ComponentHolder[C] {
  private var comp = Option.empty[C]

  final protected def component_=(c: C): Unit = {
    requireEDT()
    require(comp.isEmpty, s"Component has already been set")
    comp = Some(c)
  }

  final def component: C = {
    requireEDT()
    comp getOrElse sys.error("Called component before GUI was initialized")
  }
}
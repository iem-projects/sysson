package at.iem.sysson.sound.designer.impl

private[impl] sealed trait VisualElement {
  var name: String = ""
  var state: ElementState = ElementState.Edit
}
private object VisualGraphElem {
  def unapply(g: VisualGraphElem): Option[GraphElem] = g.content
}
private[impl] final class VisualGraphElem extends VisualElement {
  var content = Option.empty[GraphElem]
}
private[impl] final class VisualConstant extends VisualElement {
  var content = Option.empty[ConstElem]
}
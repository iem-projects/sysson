package at.iem.sysson.sound.designer.impl

private[impl] sealed trait VisualElement {
  var name: String = ""
  var state: ElementState = ElementState.Edit
}
private[impl] final class VisualGraphElem extends VisualElement {
  var content = Option.empty[GraphElem]
}
private[impl] final class VisualConstant extends VisualElement {
  var content = Option.empty[ConstElem]
}
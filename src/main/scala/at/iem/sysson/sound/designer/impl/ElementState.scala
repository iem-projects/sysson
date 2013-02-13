package at.iem.sysson.sound.designer.impl

private[impl] object ElementState {
  case object Edit  extends ElementState
  case object Error extends ElementState
  case object Ok    extends ElementState
}
private[impl] sealed trait ElementState
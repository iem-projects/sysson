package at.iem.sysson.sound.designer.impl

import collection.immutable.{IndexedSeq => IIdxSeq}
import java.awt.geom.Rectangle2D

private[impl] object VisualPorts {
  final val minSpacing = 10
}
private[impl] final class VisualPorts(numIns: Int, numOuts: Int) {
  val inlets  = IIdxSeq.fill(numIns )(new Rectangle2D.Float)
  val outlets = IIdxSeq.fill(numOuts)(new Rectangle2D.Float)
  var active  = Option.empty[Port]

  def update(bounds: Rectangle2D) {
//      val x       = bounds.getX.toFloat
//      val y       = bounds.getY.toFloat
    val x       = 0f
    val y       = 0f
    val w       = bounds.getWidth.toFloat
    val h       = bounds.getHeight.toFloat
    val wm      = w - 7
    if (numIns > 0) {
      val xf = if (numIns > 1) wm / (numIns - 1) else 0f
      var i = 0; while (i < numIns) {
        inlets(i).setRect(x + i * xf, y, 8, 3)
      i += 1 }
    }
    if (numOuts > 0) {
      val xf = if (numOuts > 1) wm / (numOuts - 1) else 0f
      val y2 = y + h - 2 /* 3 */
      var i = 0; while (i < numOuts) {
        outlets(i).setRect(x + i * xf, y2, 8, 3)
      i += 1 }
    }
  }
}

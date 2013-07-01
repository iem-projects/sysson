package at.iem.sysson

object OpenRange {
  implicit def closed(r: Range): OpenRange =
    new OpenRange(startOption = Some(r.start), endOption = Some(r.end), isInclusive = r.isInclusive, step = r.step)

  final val all = new OpenRange(None, None, isInclusive = true)

//  def at(idx: Int) = apply(Some(idx), Some(idx), isInclusive = true)
  object at {
    def apply(idx: Int) = new OpenRange(Some(idx), Some(idx), isInclusive = true)
  }
}
final case class OpenRange(startOption: Option[Int], endOption: Option[Int], isInclusive: Boolean, step: Int = 1) {
  def by(step: Int) = copy(step = step)

  def isAll: Boolean = startOption.isEmpty && stopOption.isEmpty

  /**
   * The exclusive stop position, taking care of the `isInclusive` setting
   */
  def stopOption = if (isInclusive) endOption.map(_ + 1) else endOption

  override def toString: String = {
    (startOption, endOption) match {
      case (Some(start), Some(end)) if start == end && isInclusive => s"at $start"
      case (None, None) => "all"
      case _ => genericToString
    }
  }

  private def genericToString: String = {
    val startS  = startOption.getOrElse("start")
    val endS    = endOption.getOrElse("end")
    val moveS   = if (isInclusive) "to" else "until"
    val byS     = if (step == 1) "" else s" by $step"
    s"($startS $moveS ${endS}$byS)"
  }

  /** Note: `maxStop` is always _exclusive_ */
  def toClosedRange(minStart: Int, maxStop: Int): Range = {
    val start = math.max(minStart, startOption.getOrElse(minStart))
    val end   = if (isInclusive) {
      math.min(maxStop - 1, endOption.getOrElse(maxStop - 1))
    } else {
      math.min(maxStop, endOption.getOrElse(maxStop))
    }
    (if (isInclusive) start to end else start until end) by step
  }
}
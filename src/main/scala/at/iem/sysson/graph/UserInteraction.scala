package at.iem.sysson.graph

import de.sciss.synth.Lazy

trait UserInteraction extends Lazy.Expander[Unit] {
  protected final def makeUGens = ()
}

case class UserSelectValue(v: Var /*, default: */) extends UserInteraction

case class UserSelectRange(v: Var) extends UserInteraction
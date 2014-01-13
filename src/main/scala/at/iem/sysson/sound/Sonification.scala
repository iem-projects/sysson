package at.iem.sysson
package sound

import de.sciss.lucre.{event => evt}
import evt.{Publisher, Sys}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.data.SkipList
import de.sciss.lucre.expr
import de.sciss.synth.proc.Attributes

object Sonification {
  def apply[S <: Sys[S]]: Sonification[S] = ???

  object Source {
    sealed trait Update[S <: Sys[S]]
  }
  trait Source[S <: Sys[S]] {
    def data: DataSource[S]
    // def dims: Map[String, String]
    // XXX TODO: Ops. Perhaps this could be covered by having DataSource behave like an expression?
    def dims: SkipList.Map[S, String, Expr[S, String]]
  }

  sealed trait Update[S <: Sys[S]]
}
trait Sonification[S <: Sys[S]] extends evt.Node[S] with Publisher[S, Sonification.Update[S]] {
  def patch: Patch[S] // PatchOLD.Source

  def sources: expr.Map[S, String, Sonification.Source[S], Sonification.Source.Update[S]]

  /** A scalar attribute map */
  def attributes: Attributes.Modifiable[S]
}

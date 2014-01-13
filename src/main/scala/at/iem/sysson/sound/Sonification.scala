package at.iem.sysson
package sound

import de.sciss.lucre.event.{Publisher, Sys}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.Mutable
import de.sciss.lucre.data.SkipList
import de.sciss.lucre.expr

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
trait Sonification[S <: Sys[S]] extends Mutable[S#ID, S#Tx] with Publisher[S, Sonification.Update[S]] {
  def name : Expr[S, String]
  // def spec : SonificationSpec // XXX TODO: unite with Patch, and make Expr.Var or itself mutable
  def patch: Patch[S] // PatchOLD.Source

  def sources: expr.Map[S, String, Sonification.Source[S], Sonification.Source.Update[S]]
}

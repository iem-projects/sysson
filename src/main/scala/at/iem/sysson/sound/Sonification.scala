package at.iem.sysson
package sound

import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.{LinkedList, Expr}
import de.sciss.lucre.stm.Mutable
import de.sciss.lucre.data.SkipList

object Sonification {
  trait Source[S <: Sys[S]] {
    def data: DataSource[S]
    // def dims: Map[String, String]
    def dims: SkipList.Map[S, String, Expr[S, String]]
  }
}
trait Sonification[S <: Sys[S]] extends Mutable[S#ID, S#Tx] {
  def name : Expr[S, String]
  def spec : SonificationSpec // XXX TODO: unite with Patch, and make Expr.Var or itself mutable
  def patch: Patch.Source

  // def sources: LinkedList.Modifiable[S, Sonification.Source[S], Unit]
  def sources: SkipList.Map[S, String, Sonification.Source[S]]
}

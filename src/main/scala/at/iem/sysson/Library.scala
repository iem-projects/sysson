package at.iem.sysson

import de.sciss.lucre.synth.Sys
import de.sciss.lucre.expr.Expr

object Library {
  def apply[S <: Sys[S]](implicit tx: S#Tx): Library[S] = ???

  sealed trait NodeLike[S <: Sys[S]] {
    def name: Expr.Var[S, String]
  }

  trait Branch[S <: Sys[S]] extends TreeLike.BranchLike[S, Branch[S], Leaf[S]] with NodeLike[S]
  trait Leaf  [S <: Sys[S]] extends NodeLike[S]
}
trait Library[S <: Sys[S]] extends TreeLike[S, Unit, Unit, Library[S]] {
  type Leaf   = Library.Leaf  [S]
  type Branch = Library.Branch[S]
}

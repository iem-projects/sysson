package at.iem.sysson

import de.sciss.lucre.synth.Sys
import de.sciss.lucre.expr.Expr

object Library {
  def apply[S <: Sys[S]](implicit tx: S#Tx): Library[S] = ???

  sealed trait NodeLike[S <: Sys[S]] {
    def name: Expr[S, String]
  }

  trait Branch[S <: Sys[S]] extends TreeLike.Branch[S, Branch[S], Leaf[S]] with NodeLike[S] {
    def insertLeaf  (idx: Int, init: Expr[S, String])(implicit tx: S#Tx): Leaf  [S]
    def insertBranch(idx: Int, init: Expr[S, String])(implicit tx: S#Tx): Branch[S]
  }
  trait Leaf[S <: Sys[S]] extends NodeLike[S] {
    // def name: Expr.Var[S, String]
  }
}
trait Library[S <: Sys[S]] extends TreeLike[S, Unit, Unit, Library[S]] {
  type Leaf   = Library.Leaf  [S]
  type Branch = Library.Branch[S]
}

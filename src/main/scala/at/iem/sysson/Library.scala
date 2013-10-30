package at.iem.sysson

import de.sciss.lucre.synth.Sys
import de.sciss.lucre.expr.Expr
import impl.{LibraryImpl => Impl}
import de.sciss.lucre.stm
import de.sciss.model.Change
import de.sciss.lucre.event.EventLike
import de.sciss.serial.Writable

object Library {
  def apply[S <: Sys[S]](implicit tx: S#Tx): Library[S] = Impl[S]

  sealed trait NodeLike[S <: Sys[S]] extends Writable {
    def name: Expr[S, String]
  }

  sealed trait LeafChange
  // sealed trait BranchChange
  case class Renamed      (change: Change[String]) extends LeafChange // with BranchChange
  case class SourceChanged(change: Change[String]) extends LeafChange

  // case class LeafUpdate[S <: Sys[S]](leaf: Leaf[S], changes: Vec[LeafChange])
  type LeafUpdate = Vec[LeafChange]

  type Update       [S <: Sys[S]] = TreeLike.Update      [S, Renamed, LeafUpdate, Library[S], Branch[S], Leaf[S]]
  type BranchChange [S <: Sys[S]] = TreeLike.BranchChange[S, Renamed, LeafUpdate            , Branch[S], Leaf[S]]
  type LeafChanged  [S <: Sys[S]] = TreeLike.LeafChanged [S, Renamed, LeafUpdate            , Branch[S], Leaf[S]]
  type BranchUpdate [S <: Sys[S]] = TreeLike.BranchUpdate[S, Renamed, LeafUpdate            , Branch[S], Leaf[S]]

  trait Branch[S <: Sys[S]] extends TreeLike.Branch[S, Branch[S], Leaf[S]] with NodeLike[S] {
    def insertLeaf  (idx: Int, name: Expr[S, String], source: Expr[S, String])(implicit tx: S#Tx): Leaf[S]
    def insertBranch(idx: Int, name: Expr[S, String])(implicit tx: S#Tx): Branch[S]

    // def changed: EventLike[S, BranchUpdate[S]]
  }
  trait Leaf[S <: Sys[S]] extends NodeLike[S] {
    def name  : Expr.Var[S, String]
    def source: Expr.Var[S, String]

    // def changed: EventLike[S, LeafChanged[S]]
  }
}
trait Library[S <: Sys[S]]
  extends TreeLike[S, Library.Renamed, Library.LeafUpdate, Library[S]] with stm.Mutable[S#ID, S#Tx] {

  type Leaf   = Library.Leaf  [S]
  type Branch = Library.Branch[S]
}

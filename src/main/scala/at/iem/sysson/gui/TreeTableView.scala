package at.iem.sysson
package gui

import de.sciss.lucre.event.Sys

object TreeTableView {
  import TreeLike.Node

  // trait ElementView
  
  trait Config[S <: Sys[S], T <: TreeLike[S, _, _, T]] {
    def idView: Node[T#Branch, T#Leaf] => S#ID
  }
}
trait TreeTableView[S <: Sys[S], T <: TreeLike[S, _, _, T]] {

}

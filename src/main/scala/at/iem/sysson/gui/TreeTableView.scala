package at.iem.sysson
package gui

import de.sciss.lucre.event.Sys
import impl.{TreeTableViewImpl => Impl}
import de.sciss.treetable.{TreeTable, TreeTableCellRenderer, TreeColumnModel}
import scala.swing.Component

object TreeTableView {
  trait Config[S <: Sys[S], T <: TreeLike[S, T], D] {
    type Node = TreeLike.Node[T#Branch, T#Leaf]

    def nodeID  (node: Node)(implicit tx: S#Tx): S#ID
    def viewData(node: Node)(implicit tx: S#Tx): D

    def columns: TreeColumnModel[D]

    def renderer(view: TreeTableView[S, T, D], data: D, row: Int, column: Int,
                 state: TreeTableCellRenderer.State): Component
  }

  //  def editable[S <: Sys[S], T <: TreeLike[S, T]](tree: T, config: Config[S, T])
  //                                               (implicit tx: S#Tx, cursor: stm.Cursor[S]): TreeTableView[S, T] = ???

  def apply[S <: Sys[S], T <: TreeLike[S, T], D](tree: T, config: Config[S, T, D])
                                             (implicit tx: S#Tx): TreeTableView[S, T, D] = Impl(tree, config)
}
trait TreeTableView[S <: Sys[S], T <: TreeLike[S, T], D] {
  def treeTable: TreeTable[_, _]
}

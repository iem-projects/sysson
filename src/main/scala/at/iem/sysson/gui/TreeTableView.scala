package at.iem.sysson
package gui

import de.sciss.lucre.event.Sys
import impl.{TreeTableViewImpl => Impl}
import de.sciss.treetable.{TreeTable, TreeTableCellRenderer, TreeColumnModel}
import scala.swing.Component

object TreeTableView {
  trait Handler[S <: Sys[S], T <: TreeLike[S, T], D] {
    type Node = TreeLike.Node[T#Branch, T#Leaf]

    def nodeID  (node: Node)(implicit tx: S#Tx): S#ID
    def viewData(node: Node)(implicit tx: S#Tx): D

    def columns: TreeColumnModel[D]

    def renderer(view: TreeTableView[S, T, D], data: D, row: Int, column: Int,
                 state: TreeTableCellRenderer.State): Component

    /** Notifies the handler that a branch has seen an update.
      *
      * @param  branch  the branch which has been updated
      * @param  update  the type of update
      * @param  data    the previous view data
      *
      * @return the new view data. If the update does not constitute a visible change, the
      *         handle should just return the old data. If the data changes, the tree table view
      *         will store that new data and refresh the display.
      */
    def branchUpdate(branch: T#Branch, update: T#BU, data: D)(implicit tx: S#Tx): D

    /** Notifies the handler that a leaf has seen an update.
      *
      * @param  leaf    the leaf which has been updated
      * @param  update  the type of update
      * @param  data    the previous view data
      *
      * @return the new view data. If the update does not constitute a visible change, the
      *         handle should just return the old data. If the data changes, the tree table view
      *         will store that new data and refresh the display.
      */
    def leafUpdate  (leaf  : T#Leaf  , update: T#LU, data: D)(implicit tx: S#Tx): D
  }

  //  def editable[S <: Sys[S], T <: TreeLike[S, T]](tree: T, config: Config[S, T])
  //                                               (implicit tx: S#Tx, cursor: stm.Cursor[S]): TreeTableView[S, T] = ???

  def apply[S <: Sys[S], T <: TreeLike[S, T], D](tree: T, handler: Handler[S, T, D])
                                             (implicit tx: S#Tx): TreeTableView[S, T, D] = Impl(tree, handler)
}
trait TreeTableView[S <: Sys[S], T <: TreeLike[S, T], D] {
  def treeTable: TreeTable[_, _]
}

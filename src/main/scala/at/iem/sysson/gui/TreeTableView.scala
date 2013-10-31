package at.iem.sysson
package gui

import de.sciss.lucre.event.Sys
import impl.{TreeTableViewImpl => Impl}
import de.sciss.treetable.{TreeTable, TreeTableCellRenderer, TreeColumnModel}
import scala.swing.Component

object TreeTableView {
  trait Handler[S <: Sys[S], T <: TreeLike[S, T], H <: Handler[S, T, H]] {
    type D

    /** Branch view data */
    type BD <: D
    /** Leaf view data */
    type LD <: D

    // type Node = TreeLike.Node[T#Branch, T#Leaf]

    def branchID(branch: T#Branch): S#ID
    def leafID  (leaf  : T#Leaf  ): S#ID

    def branchData(branch: T#Branch)(implicit tx: S#Tx): H#BD
    def leafData  (leaf  : T#Leaf  )(implicit tx: S#Tx): H#LD

    def columns: TreeColumnModel[H#D]

    def branchRenderer(view: TreeTableView[S, T, H], data: H#BD, row: Int, column: Int,
                       state: TreeTableCellRenderer.State): Component

    def leafRenderer  (view: TreeTableView[S, T, H], data: H#LD, row: Int, column: Int,
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
    def branchUpdate(branch: T#Branch, update: T#BU, data: H#BD)(implicit tx: S#Tx): H#BD

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
    def leafUpdate(leaf: T#Leaf, update: T#LU, data: H#LD)(implicit tx: S#Tx): H#LD
  }

  //  def editable[S <: Sys[S], T <: TreeLike[S, T]](tree: T, config: Config[S, T])
  //                                               (implicit tx: S#Tx, cursor: stm.Cursor[S]): TreeTableView[S, T] = ???

  def apply[S <: Sys[S], T <: TreeLike[S, T], H <: Handler[S, T, H]](tree: T, handler: H)
                                             (implicit tx: S#Tx): TreeTableView[S, T, H] = Impl(tree, handler)
}
trait TreeTableView[S <: Sys[S], T <: TreeLike[S, T], H <: TreeTableView.Handler[S, T, H]] {
  def component: Component
  def treeTable: TreeTable[_, _]
}

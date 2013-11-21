package at.iem.sysson

import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm

package object gui {
  // val PatchFlavor = DragAndDrop.internalFlavor[Patch]
  // val PatchSourceFlavor = DragAndDrop.internalFlavor[Patch.Source]

  // def LibraryLeafFlavor[S <: Sys[S]] = DragAndDrop.internalFlavor[stm.Source[S#Tx, Library.Leaf[S]]] // forSome { type S <: Sys[S] }

  //  def LibraryNodeFlavor[S <: Sys[S]] =
  //    DragAndDrop.internalFlavor[stm.Source[S#Tx, TreeLike.Node[Library.Branch[S], Library.Leaf[S]]]]

  val LibraryNodeFlavor = DragAndDrop.internalFlavor[LibraryNodeDrag]

  trait LibraryNodeDrag {
    type S1 <: Sys[S1]
    def cursor: stm.Cursor[S1]
    def node: stm.Source[S1#Tx, Either[Library.Branch[S1], Library.Leaf[S1]]]
  }
}

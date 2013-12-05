package at.iem.sysson
package gui

import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm
import impl.{PatchCodeFrameImpl => Impl}
import de.sciss.desktop.UndoManager

object PatchCodeFrame {
  def apply[S <: Sys[S]](entry: Library.Leaf[S], undoManager: UndoManager)
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): Unit = Impl(entry, undoManager)
}
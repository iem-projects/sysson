package at.iem.sysson
package gui

import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm
import impl.{PatchCodeFrameImpl => Impl}

object PatchCodeFrame {
  def apply[S <: Sys[S]](entry: Library.Leaf[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): Unit =
    Impl(entry)
}
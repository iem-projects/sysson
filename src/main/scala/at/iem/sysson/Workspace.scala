package at.iem.sysson

import de.sciss.lucre.{event => evt, stm}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.LinkedList
import de.sciss.file.File
import impl.{WorkspaceImpl => Impl}

object Workspace {
  // def empty[S <: Sys[S]]
  object Durable {
    def empty(dir: File): Workspace[evt.Durable] = Impl.emptyDurable(dir)
    def read (dir: File): Workspace[evt.Durable] = Impl.readDurable (dir)
  }
}
trait Workspace[S <: Sys[S]] {
  implicit def cursor: stm.Cursor[S]

  def dir: File

  def dataSources(implicit tx: S#Tx): LinkedList.Modifiable[S, DataSource[S], Unit]
}